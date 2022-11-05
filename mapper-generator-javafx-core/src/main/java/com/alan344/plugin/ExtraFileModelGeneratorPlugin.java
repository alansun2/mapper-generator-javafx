package com.alan344.plugin;

import com.alan344.bean.config.ExtraFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ExtraFileTypeEnum;
import com.alan344.utils.StringUtils;
import org.mybatis.generator.api.*;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.codegen.RootClassInfo;
import org.mybatis.generator.config.PropertyRegistry;

import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import static org.mybatis.generator.internal.util.JavaBeansUtil.getJavaBeansField;

/**
 * @author AlanSun
 * @date 2022/11/4 13:13
 **/
public class ExtraFileModelGeneratorPlugin extends PluginAdapter {

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
        return currentConfig.getExtraFileConfigs().stream()
                .filter(extraFileConfig -> extraFileConfig.getTemplateType().equals(ExtraFileTypeEnum.MODEL))
                .filter(ExtraFileConfig::isEnable).map(extraFileConfig -> {
                    // 生成类
                    TopLevelClass topLevelClass = new TopLevelClass(TableUtils.getType(introspectedTable, extraFileConfig.getPackageName(), extraFileConfig.getModelSuffix()));
                    topLevelClass.setVisibility(JavaVisibility.PUBLIC);

                    CommentGenerator commentGenerator = context.getCommentGenerator();
                    // 类注释
                    commentGenerator.addModelClassComment(topLevelClass, introspectedTable);

                    // 无主键的字段
                    List<IntrospectedColumn> introspectedColumns = introspectedTable.getNonPrimaryKeyColumns();

                    Plugin plugins = context.getPlugins();
                    String rootClass = TableUtils.getRootClass(introspectedTable, context);
                    for (IntrospectedColumn introspectedColumn : introspectedColumns) {
                        if (RootClassInfo.getInstance(rootClass, Collections.emptyList()).containsProperty(introspectedColumn)) {
                            continue;
                        }
                        // 全局忽略字段
                        if (TableUtils.isFieldIgnore(extraFileConfig.getModelIgnoreColumns(), introspectedColumn.getActualColumnName())) {
                            continue;
                        }

                        // 添加成员变量注释
                        Field field = getJavaBeansField(introspectedColumn, context, introspectedTable);
                        // 插件执行
                        if (plugins.modelFieldGenerated(field, topLevelClass, introspectedColumn, introspectedTable, ModelClassType.BASE_RECORD)) {
                            this.addValidationLengthAnnotation(extraFileConfig.isGenerateValidAnnotation(), topLevelClass, field, introspectedColumn);
                            topLevelClass.addField(field);
                            topLevelClass.addImportedType(field.getType());
                        }
                    }

                    return new GeneratedJavaFile(topLevelClass, extraFileConfig.getOutputPath(), context.getProperty(PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING), context.getJavaFormatter());
                }).collect(Collectors.toList());
    }

    /**
     * 添加 validation 注解
     */
    private void addValidationLengthAnnotation(boolean isGenerateValidationAnnotation, TopLevelClass topLevelClass, Field field, IntrospectedColumn introspectedColumn) {
        if (!isGenerateValidationAnnotation) {
            return;
        }
        String remarks = StringUtils.isNotEmpty(introspectedColumn.getRemarks()) ? introspectedColumn.getRemarks() : field.getName();
        // 使用逗号来做分割符防止注释过长
        final int i = remarks.indexOf("。");
        if (i != -1) {
            remarks = remarks.substring(0, i);
        }
        // 字符串
        if (field.getType().compareTo(FullyQualifiedJavaType.getStringInstance()) == 0) {
            final int length = introspectedColumn.getLength();
            topLevelClass.addImportedType("org.hibernate.validator.constraints.Length");
            field.addAnnotation("@Length(max = " + length + ", message = \"" + remarks + "最多 " + length + "个字符\")");

            if (!introspectedColumn.isNullable()) {
                topLevelClass.addImportedType("javax.validation.constraints.NotBlank");
                field.addAnnotation("@NotBlank(message = \"" + remarks + "必填\")");
            }
        } else {
            // 其他
            if (!introspectedColumn.isNullable()) {
                topLevelClass.addImportedType("javax.validation.constraints.NotNull");
                field.addAnnotation("@NotNull(message = \"" + remarks + "必填\")");
            }
        }
    }
}