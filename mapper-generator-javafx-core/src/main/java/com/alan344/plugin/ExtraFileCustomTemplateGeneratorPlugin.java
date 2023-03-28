package com.alan344.plugin;

import cn.hutool.core.io.FileUtil;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.tokenparse.GenericTokenParser;
import com.google.common.base.CaseFormat;
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

import static com.alan344.plugin.ExtraFileCustomTemplateGeneratorPlugin.TemplatePropertyEnum.*;

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
         * 首字母大写驼峰格式的类名
         */
        DOMAIN_UPPER_CAMEL,
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
        FIELDS_UPPER_CAMELS
    }

    private static final Map<String, Configuration> CONFIGURATION_HASH_MAP = new HashMap<>();

    private static final GenericTokenParser GENERIC_TOKEN_PARSER = new GenericTokenParser("${", "}");

    @Override
    public void setProperties(Properties properties) {
        super.setProperties(properties);
    }

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public List<GeneratedJavaFile> contextGenerateAdditionalJavaFiles(IntrospectedTable introspectedTable) {
        if (CollectionUtils.isEmpty(ConfigConstants.extraTemplateFileConfigs)) {
            return Collections.emptyList();
        }
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final LinkedHashMap<String, String> customProperties = currentConfig.getCustomProperties();
        ConfigConstants.extraTemplateFileConfigs.stream()
                .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.CUSTOM_TEMPLATE))
                .forEach(extraFileConfig -> this.process(introspectedTable, extraFileConfig, customProperties));
        return Collections.emptyList();
    }

    private void process(IntrospectedTable introspectedTable, ExtraTemplateFileConfig extraTemplateFileConfig, LinkedHashMap<String, String> customProperties) {
        final Configuration cfg = this.getConfig(extraTemplateFileConfig.getCustomTemplateDir());
        final Template template = this.getTemplate(cfg, extraTemplateFileConfig.getCustomTemplateDir());
        try {
            // 数据
            final Map<String, Object> modelData = this.prepareModelData(introspectedTable, extraTemplateFileConfig, customProperties);
            FileWriterWithEncoding fileWriterWithEncoding = new FileWriterWithEncoding(this.getFileName(extraTemplateFileConfig, modelData), StandardCharsets.UTF_8);
            template.process(modelData, fileWriterWithEncoding);
        } catch (IOException | TemplateException e) {
            throw new RuntimeException(e);
        }
    }

    private String getFileName(ExtraTemplateFileConfig extraTemplateFileConfig, Map<String, Object> modelData) {
        final String upperCamel = modelData.get(TYPE_NAME_UPPER_CAMEL.name()).toString();

        final String packageName = modelData.get(PACKAGE.name()).toString();
        final String outPathFromPackage = packageName.replaceAll("\\.", "/");

        String outputPath = extraTemplateFileConfig.getOutputPath();
        outputPath = outputPath.endsWith("/") ? outputPath : outputPath + "/";
        final File file = new File(outputPath + outPathFromPackage + "/");
        if (!file.exists()) {
            file.mkdirs();
        }
        return outputPath + outPathFromPackage + "/" + upperCamel + extraTemplateFileConfig.getModelSuffix() + ".java";
    }

    private Map<String, Object> prepareModelData(IntrospectedTable introspectedTable,
                                                 ExtraTemplateFileConfig extraTemplateFileConfig,
                                                 LinkedHashMap<String, String> customProperties) {
        HashMap<String, Object> modelDataMap = new HashMap<>(16);
        final String upperCamel = PluginUtils.getUpperCamel(introspectedTable);
        modelDataMap.put(TYPE_NAME_UPPER_CAMEL.name(), upperCamel);
        modelDataMap.put(TYPE_NAME_LOWER_CAMEL.name(), PluginUtils.getLowerCase(upperCamel));
        modelDataMap.put(TYPE_NAME_LOWER_HYPHEN.name(), PluginUtils.getLowerHyphen(upperCamel));
        modelDataMap.put(CUR_DATE_TIME.name(), LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        final PluginUtils.Domain domain = PluginUtils.getDomainFromRemarks(introspectedTable.getRemarks(), true);
        modelDataMap.put(DOMAIN.name(), domain.getD());
        modelDataMap.put(DOMAIN_UPPER_CAMEL.name(), CaseFormat.LOWER_CAMEL.to(CaseFormat.UPPER_CAMEL, domain.getD()));
        modelDataMap.put(DOMAIN_DESC.name(), domain.getDd());
        modelDataMap.put(PACKAGE.name(), this.getPackage(extraTemplateFileConfig, domain));
        final List<IntrospectedColumn> allColumns = introspectedTable.getAllColumns();
        final List<String> fieldUpperCamels = allColumns.stream().map(introspectedColumn -> CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, introspectedColumn.getActualColumnName()))
                .collect(Collectors.toList());
        modelDataMap.put(FIELDS_UPPER_CAMELS.name(), fieldUpperCamels);

        if (null != customProperties) {
            modelDataMap.putAll(customProperties);
            modelDataMap.putAll(ConfigConstants.internalGlobalParam);
        }

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
        packageName = GENERIC_TOKEN_PARSER.parse(packageName, var1 -> domain.getD());
        packageName = packageName.replaceAll("\\.\\.", "\\.");
        return packageName;
    }

    private Template getTemplate(Configuration cfg, String templateName) {
        try {
            return cfg.getTemplate(FileUtil.getName(templateName));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Configuration getConfig(String filePath) {
        Configuration cfg = CONFIGURATION_HASH_MAP.get(filePath);
        if (null == cfg) {
            final String dir = FileUtils.getFile(filePath).getParent();
            // Create your Configuration instance, and specify if up to what FreeMarker
            // version (here 2.3.29) do you want to apply the fixes that are not 100%
            // backward-compatible. See the Configuration JavaDoc for details.
            cfg = new Configuration(Configuration.VERSION_2_3_31);

            // Specify the source where the template files come from. Here I set a
            // plain directory for it, but non-file-system sources are possible too:
            ResourceLoader resourceLoader = new DefaultResourceLoader();

            cfg.setTemplateLoader(new SpringTemplateLoader(resourceLoader, dir));

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