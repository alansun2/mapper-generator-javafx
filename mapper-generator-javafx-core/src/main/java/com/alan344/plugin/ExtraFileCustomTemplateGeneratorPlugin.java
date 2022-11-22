package com.alan344.plugin;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.ExtraFileTypeEnum;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.tokenparse.GenericTokenParser;
import com.google.common.base.CaseFormat;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import org.apache.commons.io.output.FileWriterWithEncoding;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;

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
        if (CollectionUtils.isEmpty(ConfigConstants.extraFileConfigs)) {
            return Collections.emptyList();
        }

        ConfigConstants.extraFileConfigs.stream()
                .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.CUSTOM_TEMPLATE))
                .filter(ExtraFileConfig::isEnable).forEach(extraFileConfig -> this.process(introspectedTable, extraFileConfig));
        return Collections.emptyList();
    }

    private void process(IntrospectedTable introspectedTable, ExtraFileConfig extraFileConfig) {
        final Configuration cfg = this.getConfig(extraFileConfig.getCustomTemplateDir());
        final Template template = this.getTemplate(cfg, extraFileConfig.getCustomTemplateFileName());
        try {
            // 数据
            final Map<String, Object> modelData = this.prepareModelData(introspectedTable, extraFileConfig);
            FileWriterWithEncoding fileWriterWithEncoding = new FileWriterWithEncoding(this.getFileName(extraFileConfig, modelData), StandardCharsets.UTF_8);
            template.process(modelData, fileWriterWithEncoding);
        } catch (IOException | TemplateException e) {
            throw new RuntimeException(e);
        }
    }

    private String getFileName(ExtraFileConfig extraFileConfig, Map<String, Object> modelData) {
        final String upperCamel = modelData.get(TYPE_NAME_UPPER_CAMEL.name()).toString();

        final String packageName = modelData.get(PACKAGE.name()).toString();
        final String outPathFromPackage = packageName.replaceAll("\\.", "/");

        String outputPath = extraFileConfig.getOutputPath();
        outputPath = outputPath.endsWith("/") ? outputPath : outputPath + "/";
        final File file = new File(outputPath + outPathFromPackage + "/");
        if (!file.exists()) {
            file.mkdirs();
        }
        return outputPath + outPathFromPackage + "/" + upperCamel + extraFileConfig.getModelSuffix() + ".java";
    }

    private Map<String, Object> prepareModelData(IntrospectedTable introspectedTable, ExtraFileConfig extraFileConfig) {
        HashMap<String, Object> modelDataMap = new HashMap<>(16);
        final String upperCamel = TableUtils.getUpperCamel(introspectedTable);
        modelDataMap.put(TYPE_NAME_UPPER_CAMEL.name(), upperCamel);
        modelDataMap.put(TYPE_NAME_LOWER_CAMEL.name(), TableUtils.getLowerCase(upperCamel));
        modelDataMap.put(TYPE_NAME_LOWER_HYPHEN.name(), TableUtils.getLowerHyphen(upperCamel));
        modelDataMap.put(CUR_DATE_TIME.name(), LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        final TableUtils.Domain domain = TableUtils.getDomainFromRemarks(introspectedTable.getRemarks(), true);
        modelDataMap.put(DOMAIN.name(), domain.getD());
        modelDataMap.put(DOMAIN_UPPER_CAMEL.name(), CaseFormat.LOWER_CAMEL.to(CaseFormat.UPPER_CAMEL, domain.getD()));
        modelDataMap.put(DOMAIN_DESC.name(), domain.getDd());
        modelDataMap.put(PACKAGE.name(), this.getPackage(extraFileConfig, domain));
        final List<IntrospectedColumn> allColumns = introspectedTable.getAllColumns();
        final List<String> fieldUpperCamels = allColumns.stream().map(introspectedColumn -> CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, introspectedColumn.getActualColumnName()))
                .collect(Collectors.toList());
        modelDataMap.put(FIELDS_UPPER_CAMELS.name(), fieldUpperCamels);
        return modelDataMap;
    }

    /**
     * 获取包名
     *
     * @param extraFileConfig 配置
     * @param domain          领域
     * @return 包名
     */
    private String getPackage(ExtraFileConfig extraFileConfig, TableUtils.Domain domain) {
        String packageName = extraFileConfig.getPackageName();
        packageName = GENERIC_TOKEN_PARSER.parse(packageName, var1 -> domain.getD());
        packageName = packageName.replaceAll("\\.\\.", "\\.");
        return packageName;
    }

    private Template getTemplate(Configuration cfg, String templateName) {
        try {
            return cfg.getTemplate(templateName);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Configuration getConfig(String templateDir) {
        Configuration cfg = CONFIGURATION_HASH_MAP.get(templateDir);
        if (null == cfg) {
            // Create your Configuration instance, and specify if up to what FreeMarker
            // version (here 2.3.29) do you want to apply the fixes that are not 100%
            // backward-compatible. See the Configuration JavaDoc for details.
            cfg = new Configuration(Configuration.VERSION_2_3_31);

            // Specify the source where the template files come from. Here I set a
            // plain directory for it, but non-file-system sources are possible too:
            try {
                cfg.setDirectoryForTemplateLoading(new File(templateDir));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

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
            CONFIGURATION_HASH_MAP.put(templateDir, cfg);
        }
        return cfg;
    }
}