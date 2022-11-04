package com.alan344.plugin;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ExtraFileTypeEnum;
import org.apache.commons.io.FileUtils;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/4 13:13
 **/
public class ExtraFileCustomTemplateGeneratorPlugin extends PluginAdapter {

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
                .filter(extraFileConfig -> extraFileConfig.getTemplateType().equals(ExtraFileTypeEnum.MODEL))
                .filter(ExtraFileConfig::isEnable).map(extraFileConfig -> {
                    final String customTemplateInputPath = extraFileConfig.getCustomTemplateInputPath();
                    String templateContent;
                    try {
                        templateContent = FileUtils.readFileToString(new File(customTemplateInputPath), StandardCharsets.UTF_8);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }

//                    return new GeneratedJavaFile(topLevelClass, context.getJavaModelGeneratorConfiguration().getTargetProject(), context.getProperty(PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING), context.getJavaFormatter());
                    return null;
                }).collect(Collectors.toList());
        return null;
    }
}