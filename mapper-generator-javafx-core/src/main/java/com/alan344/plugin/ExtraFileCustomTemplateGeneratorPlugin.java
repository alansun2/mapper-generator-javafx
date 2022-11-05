package com.alan344.plugin;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ExtraFileTypeEnum;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import org.apache.commons.io.output.FileWriterWithEncoding;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author AlanSun
 * @date 2022/11/4 13:13
 **/
public class ExtraFileCustomTemplateGeneratorPlugin extends PluginAdapter {

    private enum TemplatePropertyEnum {
        /**
         * 首字母大写的类名
         */
        TYPE_NAME,
        /**
         * 首字母小写的类名
         */
        TYPE_NAME_FIRST_LETTER_LOWERCASE
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

    @Override
    public List<GeneratedJavaFile> contextGenerateAdditionalJavaFiles(IntrospectedTable introspectedTable) {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        currentConfig.getExtraFileConfigs().stream()
                .filter(extraFileConfig -> extraFileConfig.getTemplateType().equals(ExtraFileTypeEnum.CUSTOM_TEMPLATE))
                .filter(ExtraFileConfig::isEnable).forEach(extraFileConfig -> this.process(introspectedTable, extraFileConfig));
        return Collections.emptyList();
    }

    private void process(IntrospectedTable introspectedTable, ExtraFileConfig extraFileConfig) {
        final Configuration cfg = this.getConfig(this.getDirFromPath(extraFileConfig.getCustomTemplateInputPath()));
        final Template template = this.getTemplate(cfg, this.getTemplateName(extraFileConfig.getCustomTemplateInputPath()));
        try {
            FileWriterWithEncoding fileWriterWithEncoding = new FileWriterWithEncoding(extraFileConfig.getOutputPath(), StandardCharsets.UTF_8);
            template.process(this.prepareModelData(introspectedTable), fileWriterWithEncoding);
        } catch (IOException | TemplateException e) {
            throw new RuntimeException(e);
        }
    }

    private Map<String, Object> prepareModelData(IntrospectedTable introspectedTable) {
        HashMap<String, Object> map = new HashMap<>(16);
        map.put(TemplatePropertyEnum.TYPE_NAME.name(), TableUtils.getOriginalBeanName(introspectedTable));
        map.put(TemplatePropertyEnum.TYPE_NAME_FIRST_LETTER_LOWERCASE.name(), TableUtils.firstLetterLowercase(TableUtils.getOriginalBeanName(introspectedTable)));
        return map;
    }

    private String getDirFromPath(String filePath) {
        File file = new File(filePath);
        return file.getParent();
    }

    private String getTemplateName(String filePath) {
        File file = new File(filePath);
        return file.getName();
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