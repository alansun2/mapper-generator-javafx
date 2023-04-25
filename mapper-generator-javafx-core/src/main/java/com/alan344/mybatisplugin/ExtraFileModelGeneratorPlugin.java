package com.alan344.mybatisplugin;

import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.StringUtils;
import org.mybatis.generator.api.*;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.codegen.RootClassInfo;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.internal.util.StringUtility;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.mybatis.generator.internal.util.JavaBeansUtil.getJavaBeansField;

/**
 * @author AlanSun
 * @date 2022/11/4 13:13
 **/
public class ExtraFileModelGeneratorPlugin extends PluginAdapter {

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

        return ConfigConstants.extraTemplateFileConfigs.stream()
                .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.MODEL))
                .map(extraFileConfig -> {
                    // 生成类
                    TopLevelClass topLevelClass = new TopLevelClass(PluginUtils.getTypeFullName(introspectedTable, this.getPackageName(introspectedTable.getRemarks(), extraFileConfig), extraFileConfig.getModelSuffix()));
                    topLevelClass.setVisibility(JavaVisibility.PUBLIC);
                    // 添加 lombok 注解
                    this.addLombok(extraFileConfig, topLevelClass);

                    // 父类
                    if (StringUtility.stringHasValue(extraFileConfig.getSuperClass())) {
                        topLevelClass.setSuperClass(extraFileConfig.getSuperClass());
                    }

                    CommentGenerator commentGenerator = context.getCommentGenerator();
                    // 类注释
                    commentGenerator.addModelClassComment(topLevelClass, introspectedTable);

                    // 无主键的字段
                    List<IntrospectedColumn> introspectedColumns = introspectedTable.getAllColumns();

                    Plugin plugins = context.getPlugins();
                    String rootClass = PluginUtils.getRootClass(introspectedTable, context);
                    for (IntrospectedColumn introspectedColumn : introspectedColumns) {
                        if (RootClassInfo.getInstance(rootClass, Collections.emptyList()).containsProperty(introspectedColumn)) {
                            continue;
                        }
                        // 全局忽略字段
                        if (PluginUtils.isFieldIgnore(extraFileConfig.getModelIgnoreColumns(), introspectedColumn.getActualColumnName())) {
                            continue;
                        }

                        // 添加成员变量注释
                        Field field = getJavaBeansField(introspectedColumn, context, introspectedTable);

                        // 插件执行
                        if (plugins.modelFieldGenerated(field, topLevelClass, introspectedColumn, introspectedTable, ModelClassType.BASE_RECORD)) {
                            // 添加 validate 注解
                            this.addValidationLengthAnnotation(extraFileConfig.isGenerateValidAnnotation(), topLevelClass, field, introspectedColumn);
                            topLevelClass.addField(field);
                            topLevelClass.addImportedType(field.getType());
                        }
                    }

                    return new GeneratedJavaFile(topLevelClass, StrUtil.addSuffixIfNot(currentConfig.getProjectDir(), StrUtil.SLASH) + extraFileConfig.getOutputPath(), context.getProperty(PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING), context.getJavaFormatter());
                }).collect(Collectors.toList());
    }

    /**
     * 获取包名
     *
     * @param remarks                 备注
     * @param extraTemplateFileConfig 配置文件
     * @return 包名
     */
    private String getPackageName(String remarks, ExtraTemplateFileConfig extraTemplateFileConfig) {
        String packageName = extraTemplateFileConfig.getPackageName();

        final PluginUtils.Domain domain = PluginUtils.getDomainFromRemarks(remarks, true);
        packageName = PluginUtils.parse(packageName, domain);
        packageName = packageName.replace("..", ".");
        return packageName;
    }

    /**
     * 添加 lombok 注解
     *
     * @param topLevelClass {@link TopLevelClass}
     */
    private void addLombok(ExtraTemplateFileConfig extraTemplateFileConfig, TopLevelClass topLevelClass) {
//        if (data) {
//            topLevelClass.addImportedType("lombok.Data");
//            topLevelClass.addAnnotation("@Data");
//        }
        if (extraTemplateFileConfig.isLombokGetter()) {
            topLevelClass.addImportedType("lombok.Getter");
            topLevelClass.addAnnotation("@Getter");
        }
        if (extraTemplateFileConfig.isLombokSetter()) {
            topLevelClass.addImportedType("lombok.Setter");
            topLevelClass.addAnnotation("@Setter");
        }
        if (extraTemplateFileConfig.isLombokToString()) {
            topLevelClass.addImportedType("lombok.ToString");
            topLevelClass.addAnnotation("@ToString");
        }
//        if (!data && equalsAndHashCode) {
//            topLevelClass.addImportedType("lombok.EqualsAndHashCode");
//            topLevelClass.addAnnotation("@EqualsAndHashCode");
//        }
//        if (builder) {
//            topLevelClass.addImportedType("lombok.Builder");
//            topLevelClass.addAnnotation("@Builder");
//        }
//        if (noArgsConstructor) {
//            topLevelClass.addImportedType("lombok.NoArgsConstructor");
//            topLevelClass.addAnnotation("@NoArgsConstructor");
//        }
//
//        if (allArgsConstructor) {
//            topLevelClass.addImportedType("lombok.AllArgsConstructor");
//            topLevelClass.addAnnotation("@AllArgsConstructor");
//        }
//        if (!data && requiredArgsConstructor) {
//            topLevelClass.addImportedType("lombok.RequiredArgsConstructor");
//            topLevelClass.addAnnotation("@RequiredArgsConstructor");
//        }
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
            field.addAnnotation("@Length(max = " + length + ", message = \"" + remarks + "最多 " + length + " 个字符\")");

            if (!introspectedColumn.isNullable()) {
                topLevelClass.addImportedType("javax.validation.constraints.NotBlank");
                field.addAnnotation("@NotBlank(message = \"" + remarks + " 必填\")");
            }
        } else {
            // 其他
            if (!introspectedColumn.isNullable()) {
                topLevelClass.addImportedType("javax.validation.constraints.NotNull");
                field.addAnnotation("@NotNull(message = \"" + remarks + " 必填\")");
            }
        }
    }
}