package com.alan344.mybatisplugin;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.enums.ExtraFileTypeEnum;
import com.alan344.utils.CollectionUtils;
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
import org.mybatis.generator.internal.util.StringUtility;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.mybatis.generator.internal.util.JavaBeansUtil.getJavaBeansField;

/**
 * @author AlanSun
 * @since 2022/11/4 13:13
 **/
public class ExtraFileJPAlGeneratorPlugin extends PluginAdapter {

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
                .filter(extraFileConfig -> extraFileConfig.getExtraFileType().equals(ExtraFileTypeEnum.JPA_ENTITY))
                .map(extraFileConfig -> {
                    // 生成类
                    TopLevelClass topLevelClass = new TopLevelClass(PluginUtils.getTypeFullName(introspectedTable,
                            this.getPackageName(introspectedTable.getRemarks(), extraFileConfig),
                            extraFileConfig.getModelSuffix()));
                    topLevelClass.setVisibility(JavaVisibility.PUBLIC);
                    // 添加 lombok 注解
                    this.addLombok(extraFileConfig, topLevelClass);

                    // 父类
                    final String superClass = extraFileConfig.getSuperClass();
                    if (StringUtility.stringHasValue(superClass)) {
                        final String name = superClass.substring(superClass.lastIndexOf(".") + 1);
                        topLevelClass.setSuperClass(name);
                        topLevelClass.addImportedType(superClass);
                    }

                    CommentGenerator commentGenerator = context.getCommentGenerator();
                    // 类注释
                    commentGenerator.addModelClassComment(topLevelClass, introspectedTable);

                    // 无主键的字段
                    List<IntrospectedColumn> introspectedColumns = introspectedTable.getAllColumns();

                    //
                    topLevelClass.addAnnotation("@Entity");
                    topLevelClass.addImportedType("jakarta.persistence.Entity");

                    topLevelClass.addAnnotation("@Table(name = \"" + introspectedTable.getFullyQualifiedTable().getIntrospectedTableName() +
                                                "\")");
                    topLevelClass.addImportedType("jakarta.persistence.Table");

                    Plugin plugins = context.getPlugins();
                    String rootClass = PluginUtils.getRootClass(introspectedTable, context);
                    for (IntrospectedColumn introspectedColumn : introspectedColumns) {
                        if (RootClassInfo.getInstance(rootClass, Collections.emptyList()).containsProperty(introspectedColumn)) {
                            continue;
                        }
                        // 全局忽略字段
                        if (PluginUtils.isFieldIgnore(extraFileConfig.getModelIgnoreColumns(),
                                introspectedColumn.getActualColumnName())) {
                            continue;
                        }

                        // 添加成员变量注释
                        Field field = getJavaBeansField(introspectedColumn, context, introspectedTable);

                        // 插件执行
                        if (plugins.modelFieldGenerated(field, topLevelClass, introspectedColumn, introspectedTable,
                                ModelClassType.BASE_RECORD)) {
                            // 添加 validate 注解
                            this.addValidationLengthAnnotation(extraFileConfig.isGenerateValidAnnotation(),
                                    topLevelClass, field, introspectedColumn);
                            this.addColumnAnnotation(topLevelClass, field, introspectedColumn);
                            topLevelClass.addField(field);
                            topLevelClass.addImportedType(field.getType());
                        }
                    }

                    final String dir =
                            StrUtil.addSuffixIfNot(currentConfig.getProjectDir(), StrUtil.SLASH) + extraFileConfig.getOutputPath();
                    if (!FileUtil.exist(dir)) {
                        FileUtil.mkdir(dir);
                    }
                    return new GeneratedJavaFile(topLevelClass,
                            dir,
                            context.getProperty(PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING),
                            context.getJavaFormatter());
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
     * 添加 validation 注解
     */
    private void addValidationLengthAnnotation(boolean isGenerateValidationAnnotation, TopLevelClass topLevelClass,
                                               Field field, IntrospectedColumn introspectedColumn) {
        if (!isGenerateValidationAnnotation) {
            return;
        }

        // 字符串
        if (field.getType().compareTo(FullyQualifiedJavaType.getStringInstance()) == 0) {
            final int length = introspectedColumn.getLength();
            topLevelClass.addImportedType("jakarta.validation.constraints.Size");
            field.addAnnotation("@Size(max = " + length + ")");
        }

        // 其他
        if (!introspectedColumn.isNullable()) {
            topLevelClass.addImportedType("javax.validation.constraints.NotNull");
            field.addAnnotation("@NotNull");
        }
    }

    private void addColumnAnnotation(TopLevelClass topLevelClass,
                                     Field field, IntrospectedColumn introspectedColumn) {
        topLevelClass.addImportedType("jakarta.persistence.Column");
        StringBuilder sb = new StringBuilder();
        sb.append("@Column(name = \"").append(introspectedColumn.getActualColumnName()).append("\"");
        if (!introspectedColumn.isNullable()) {
            sb.append(", nullable = false");
        }
        sb.append(")");
        field.addAnnotation(sb.toString());
    }
}