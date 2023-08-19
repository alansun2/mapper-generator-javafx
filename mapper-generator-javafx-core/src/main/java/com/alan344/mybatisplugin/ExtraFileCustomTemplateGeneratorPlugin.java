package com.alan344.mybatisplugin;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.StringUtils;
import com.google.common.base.CaseFormat;
import freemarker.cache.MultiTemplateLoader;
import freemarker.cache.TemplateLoader;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.output.FileWriterWithEncoding;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.ResourceLoader;
import org.springframework.ui.freemarker.SpringTemplateLoader;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import static com.alan344.mybatisplugin.ExtraFileCustomTemplateGeneratorPlugin.TemplatePropertyEnum.*;

/**
 * @author AlanSun
 * @date 2022/11/4 13:13
 **/
public class ExtraFileCustomTemplateGeneratorPlugin extends PluginAdapter {

    public enum TemplatePropertyEnum {
        /**
         * package
         */
        PACKAGE,
        /**
         * 首字母大写驼峰格式的类名
         */
        TYPE_NAME_UPPER_CAMEL,
        /**
         * 首字母小写驼峰格式的类名
         */
        TYPE_NAME_LOWER_CAMEL,
        /**
         * 中划线分割小写格式的类名
         */
        TYPE_NAME_LOWER_HYPHEN,
        /**
         * 领域
         */
        DOMAIN,
        /**
         * 领域描述
         */
        DOMAIN_DESC,
        /**
         * 当前时间
         */
        CUR_DATE_TIME,
        /**
         * 字段
         */
        FIELDS_UPPER_CAMELS,
        /**
         * 被忽略的字段
         */
        IGNORE_FIELDS_MAP,
        /**
         * 自定义参数 map
         */
        CUSTOM_PARAMS_MAP
    }

    private static final Map<String, Configuration> CONFIGURATION_HASH_MAP = new HashMap<>();

    @Override
    public void setProperties(Properties properties) {
        super.setProperties(properties);
    }

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    final Map<String, List<String>> modelSuffixIgnoreColumsMap = ConfigConstants.extraTemplateFileConfigs.stream()
            .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.MODEL))
            .filter(extraFileConfig -> StringUtils.isNotEmpty(extraFileConfig.getModelIgnoreColumns()))
            .collect(Collectors.toMap(ExtraTemplateFileConfig::getModelSuffix,
                    extraTemplateFileConfig -> Arrays.stream(extraTemplateFileConfig.getModelIgnoreColumns().split(","))
                            .map(s -> s.replace("_", "")).distinct().toList(), (o, o2) -> o));

    @Override
    public List<GeneratedJavaFile> contextGenerateAdditionalJavaFiles(IntrospectedTable introspectedTable) {
        if (CollectionUtils.isEmpty(ConfigConstants.extraTemplateFileConfigs)) {
            return Collections.emptyList();
        }
        final MybatisExportConfig mybatisExportConfig = BaseConstants.currentConfig;
        ConfigConstants.extraTemplateFileConfigs.stream()
                .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.CUSTOM_TEMPLATE))
                .forEach(extraFileConfig -> this.process(introspectedTable, mybatisExportConfig, extraFileConfig));
        return Collections.emptyList();
    }

    private void process(IntrospectedTable introspectedTable, MybatisExportConfig mybatisExportConfig,
                         ExtraTemplateFileConfig extraTemplateFileConfig) {
        final Configuration cfg = this.getConfig(extraTemplateFileConfig.getCustomTemplateDir());
        final Template template = this.getTemplate(cfg, extraTemplateFileConfig.getCustomTemplateDir());
        try {
            // 数据
            final Map<String, Object> modelData = this.prepareModelData(introspectedTable, extraTemplateFileConfig);
            final String fileName = this.getFileName(mybatisExportConfig, extraTemplateFileConfig, modelData);
            FileWriterWithEncoding fileWriterWithEncoding = new FileWriterWithEncoding(fileName, StandardCharsets.UTF_8);
            template.process(modelData, fileWriterWithEncoding);
        } catch (IOException | TemplateException e) {
            throw new RuntimeException(e);
        }
    }

    private String getFileName(MybatisExportConfig mybatisExportConfig, ExtraTemplateFileConfig extraTemplateFileConfig, Map<String, Object> modelData) throws IOException {
        final String upperCamel = modelData.get(TYPE_NAME_UPPER_CAMEL.name()).toString();

        final String outPathFromPackage = modelData.get(PACKAGE.name()).toString().replace(StrUtil.DOT, StrUtil.SLASH);

        String outputPath = StrUtil.addSuffixIfNot(extraTemplateFileConfig.getOutputPath(), StrUtil.SLASH);

        String suffix = outputPath.contains(":") ? "" : StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectDir(), StrUtil.SLASH);

        final File dir = new File(suffix + outputPath + outPathFromPackage + StrUtil.SLASH);
        if (!dir.exists()) {
            final boolean mkdir = dir.mkdirs();
            if (!mkdir) {
                throw new RuntimeException("创建文件夹失败: " + dir.getCanonicalPath());
            }
        }
        return dir.getPath().replace(StrUtil.BACKSLASH, StrUtil.SLASH) + StrUtil.SLASH + upperCamel + extraTemplateFileConfig.getModelSuffix() + ".java";
    }

    private Map<String, Object> prepareModelData(IntrospectedTable introspectedTable,
                                                 ExtraTemplateFileConfig extraTemplateFileConfig) {
        HashMap<String, Object> modelDataMap = new HashMap<>(16);
        final String upperCamel = PluginUtils.getUpperCamel(introspectedTable);
        modelDataMap.put(TYPE_NAME_UPPER_CAMEL.name(), upperCamel);
        modelDataMap.put(TYPE_NAME_LOWER_CAMEL.name(), PluginUtils.getLowerCase(upperCamel));
        modelDataMap.put(TYPE_NAME_LOWER_HYPHEN.name(), PluginUtils.getLowerHyphen(upperCamel));
        modelDataMap.put(CUR_DATE_TIME.name(), LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        final PluginUtils.Domain domain = PluginUtils.getDomainFromRemarks(introspectedTable.getRemarks(), true);
        modelDataMap.put(DOMAIN.name(), domain.getD());
        modelDataMap.put(DOMAIN_DESC.name(), domain.getDd());
        modelDataMap.put(PACKAGE.name(), this.getPackage(extraTemplateFileConfig, domain));
        modelDataMap.put(IGNORE_FIELDS_MAP.name(), modelSuffixIgnoreColumsMap);
        final List<IntrospectedColumn> allColumns = introspectedTable.getAllColumns();
        final List<String> fieldUpperCamels = allColumns.stream()
                .map(introspectedColumn -> CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, introspectedColumn.getActualColumnName()))
                .collect(Collectors.toList());
        modelDataMap.put(FIELDS_UPPER_CAMELS.name(), fieldUpperCamels);

        // 自定义参数
        modelDataMap.putAll(ConfigConstants.globalParam);

        final Map<String, String> namePackageMap = ConfigConstants.namePackageMap;
        namePackageMap.forEach((s, s2) -> {
            if (s.startsWith(upperCamel)) {
                namePackageMap.put(s, PluginUtils.parse(s2, domain));
            }
        });
        // 包名
        modelDataMap.put(CUSTOM_PARAMS_MAP.name(), namePackageMap);

        return modelDataMap;
    }

    /**
     * 获取包名
     *
     * @param extraTemplateFileConfig 配置
     * @param domain                  领域
     * @return 包名
     */
    private String getPackage(ExtraTemplateFileConfig extraTemplateFileConfig, PluginUtils.Domain domain) {
        String packageName = extraTemplateFileConfig.getPackageName();
        packageName = PluginUtils.parse(packageName, domain);
        packageName = packageName.replace("..", ".");
        return packageName;
    }

    /**
     * 根据模板名称获取模板
     *
     * @param cfg          配置
     * @param templateName 模板名称
     * @return 模板
     */
    private Template getTemplate(Configuration cfg, String templateName) {
        try {
            return cfg.getTemplate(FileUtil.getName(templateName));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Configuration getConfig(String filePath) {
        final String dir = FileUtils.getFile(filePath).getParent().replace(StrUtil.BACKSLASH, StrUtil.SLASH);
        Configuration cfg = CONFIGURATION_HASH_MAP.get(dir);
        if (null == cfg) {
            // Create your Configuration instance, and specify if up to what FreeMarker
            // version (here 2.3.29) do you want to apply the fixes that are not 100%
            // backward-compatible. See the Configuration JavaDoc for details.
            cfg = new Configuration(Configuration.VERSION_2_3_31);

            // Specify the source where the template files come from. Here I set a
            // plain directory for it, but non-file-system sources are possible too:
            ResourceLoader resourceLoader = new DefaultResourceLoader();

            final SpringTemplateLoader ftl1 = new SpringTemplateLoader(resourceLoader, "file:" + dir);
            final SpringTemplateLoader ftl2 = new SpringTemplateLoader(resourceLoader, "classpath:/templates/common/");
            SpringTemplateLoader ftl3 = null;
            if (filePath.contains("cola")) {
                ftl3 = new SpringTemplateLoader(resourceLoader, "classpath:/templates/cola/");
            } else if (filePath.contains("layer")) {
                ftl3 = new SpringTemplateLoader(resourceLoader, "classpath:/templates/layer/");
            }

            TemplateLoader[] loaders;
            if (ftl3 == null) {
                loaders = new TemplateLoader[]{ftl1, ftl2};
            } else {
                loaders = new TemplateLoader[]{ftl1, ftl2, ftl3};
            }
            cfg.setTemplateLoader(new MultiTemplateLoader(loaders));

            // From here we will set the settings recommended for new projects. These
            // aren't the defaults for backward compatibilty.

            // Set the preferred charset template files are stored in. UTF-8 is
            // a good choice in most applications:
            cfg.setDefaultEncoding("UTF-8");

            // Sets how errors will appear.
            // During web page *development* TemplateExceptionHandler.HTML_DEBUG_HANDLER is better.
            cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);

            // Don't log exceptions inside FreeMarker that it will thrown at you anyway:
            cfg.setLogTemplateExceptions(false);

            // Wrap unchecked exceptions thrown during template processing into TemplateException-s:
            cfg.setWrapUncheckedExceptions(true);

            // Do not fall back to higher scopes when reading a null loop variable:
            cfg.setFallbackOnNullLoopVariable(false);
            CONFIGURATION_HASH_MAP.put(dir, cfg);
        }
        return cfg;
    }
}