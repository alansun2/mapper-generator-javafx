package com.alan344.mybatisplugin;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.constants.enums.HttpParamType;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.StringUtils;
import org.mybatis.generator.api.CommentGenerator;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.Plugin;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.codegen.RootClassInfo;
import org.mybatis.generator.config.PropertyRegistry;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.mybatis.generator.internal.util.JavaBeansUtil.getJavaBeansField;

/**
 * @author AlanSun
 * @since 2022/11/4 13:13
 **/
public class ExtraFileModelGeneratorPlugin extends PluginAdapter {

    @Override
    public boolean validate(List<String> warnings) {
        return CollectionUtils.isNotEmpty(ConfigConstants.extraTemplateFileConfigs);
    }

    @Override
    public List<GeneratedJavaFile> contextGenerateAdditionalJavaFiles(IntrospectedTable introspectedTable) {
        return ConfigConstants.extraTemplateFileConfigs.stream()
                .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.MODEL))
                .map(extraFileConfig ->
                        this.getGeneratedJavaFile(introspectedTable, extraFileConfig, BaseConstants.currentConfig))
                .collect(Collectors.toList());
    }

    private GeneratedJavaFile getGeneratedJavaFile(final IntrospectedTable introspectedTable,
                                                   final ExtraTemplateFileConfig extraFileConfig,
                                                   final MybatisExportConfig currentConfig) {
        // 生成类
        TopLevelClass topLevelClass = new TopLevelClass(PluginUtils.getTypeFullName(introspectedTable,
                this.getPackageName(introspectedTable.getRemarks(), extraFileConfig), extraFileConfig.getModelSuffix()));
        topLevelClass.setVisibility(JavaVisibility.PUBLIC);
        // 添加 lombok 注解
        this.addLombok(extraFileConfig, topLevelClass);
        // 添加 SpringDoc 注解支持
        this.addSpringDocAnnotations(extraFileConfig, topLevelClass, introspectedTable);

        // 父类
        final String superClass = extraFileConfig.getSuperClass();
        if (StringUtils.isNotEmpty(superClass)) {
            final String name = superClass.substring(superClass.lastIndexOf(".") + 1);
            topLevelClass.setSuperClass(name);
            topLevelClass.addImportedType(superClass);
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
            if (plugins.modelFieldGenerated(field, topLevelClass, introspectedColumn, introspectedTable,
                    ModelClassType.BASE_RECORD)) {
                // 添加 validate 注解
                this.addValidationLengthAnnotation(extraFileConfig.isGenerateValidAnnotation(), topLevelClass,
                        field, introspectedColumn);
                // 添加 SpringDoc 字段注解
                this.addSpringDocFieldAnnotations(extraFileConfig, topLevelClass, field, introspectedColumn);
                topLevelClass.addField(field);
                topLevelClass.addImportedType(field.getType());
            }
        }

        final String dir =
                StrUtil.addSuffixIfNot(currentConfig.getProjectDir(), StrUtil.SLASH) + extraFileConfig.getOutputPath();
        if (!FileUtil.exist(dir)) {
            FileUtil.mkdir(dir);
        }

        // tinyint(1) 转 boolean
        PluginUtils.tinyInt2Boolean(topLevelClass, introspectedTable);

        return new GeneratedJavaFile(topLevelClass, dir,
                context.getProperty(PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING), context.getJavaFormatter());
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
    }

    /**
     * 添加 SpringDoc 类级别注解
     *
     * @param extraFileConfig   额外文件配置
     * @param topLevelClass     顶级类
     * @param introspectedTable 表信息
     */
    private void addSpringDocAnnotations(ExtraTemplateFileConfig extraFileConfig, TopLevelClass topLevelClass,
                                         IntrospectedTable introspectedTable) {
        // 检查是否启用 SpringDoc 支持
        if (!extraFileConfig.isGenerateSpringDocAnnotation()) {
            return;
        }

        if (extraFileConfig.getHttpParamType() == HttpParamType.PATH) {
            // 添加 @ParameterObject 注解
            topLevelClass.addImportedType("org.springdoc.api.annotations.ParameterObject");
            topLevelClass.addAnnotation("@ParameterObject");
        }
    }

    /**
     * 添加 SpringDoc 字段注解
     *
     * @param extraFileConfig    额外文件配置
     * @param topLevelClass      顶级类
     * @param field              字段
     * @param introspectedColumn 列信息
     */
    private void addSpringDocFieldAnnotations(ExtraTemplateFileConfig extraFileConfig, TopLevelClass topLevelClass, Field field
            , IntrospectedColumn introspectedColumn) {
        // 检查是否启用 SpringDoc 支持
        if (!extraFileConfig.isGenerateSpringDocAnnotation()) {
            return;
        }

        // 添加 @Schema 注解
        topLevelClass.addImportedType("io.swagger.v3.oas.annotations.media.Schema");

        String columnRemarks = introspectedColumn.getRemarks();
        if (StringUtils.isEmpty(columnRemarks)) {
            columnRemarks = field.getName();
        }

        // 添加字段描述
        String annotationBuilder = "@Schema(" + "description = \"" + columnRemarks + "\")";
        field.addAnnotation(annotationBuilder);
    }

    /**
     * 添加 validation 注解
     */
    private void addValidationLengthAnnotation(boolean isGenerateValidationAnnotation, TopLevelClass topLevelClass,
                                               Field field, IntrospectedColumn introspectedColumn) {
        if (!isGenerateValidationAnnotation) {
            return;
        }
        String remarks =
                StringUtils.isNotEmpty(introspectedColumn.getRemarks()) ? introspectedColumn.getRemarks() : field.getName();
        // 使用逗号来做分割符防止注释过长
        final int i = remarks.indexOf("。");
        if (i != -1) {
            remarks = remarks.substring(0, i);
        }
        // 字符串
        String packagePrefix = PluginUtils.getPackagePrefix();
        if (field.getType().compareTo(FullyQualifiedJavaType.getStringInstance()) == 0) {
            final int length = introspectedColumn.getLength();
            topLevelClass.addImportedType(packagePrefix + ".validation.constraints.Size");
            field.addAnnotation("@Size(max = " + length + ", message = \"" + remarks + "最多 " + length + " 个字符\")");

            if (!introspectedColumn.isNullable()) {
                topLevelClass.addImportedType(packagePrefix + ".validation.constraints.NotBlank");
                field.addAnnotation("@NotBlank(message = \"" + remarks + " 必填\")");
            }
        } else {
            // 其他
            if (!introspectedColumn.isNullable()) {
                topLevelClass.addImportedType(packagePrefix + ".validation.constraints.NotNull");
                field.addAnnotation("@NotNull(message = \"" + remarks + " 必填\")");
            }
        }
    }
}