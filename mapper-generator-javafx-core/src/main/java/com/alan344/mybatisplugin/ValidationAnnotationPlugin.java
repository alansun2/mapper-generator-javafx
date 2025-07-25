package com.alan344.mybatisplugin;

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.api.dom.kotlin.KotlinFile;
import org.mybatis.generator.api.dom.kotlin.KotlinProperty;
import org.mybatis.generator.api.dom.kotlin.KotlinType;

import java.util.List;
import java.util.Optional;

/**
 * 为Model类添加Validation注解的插件
 * 添加@NotBlank、@NotNull、@Length等注解
 *
 * @author AlanSun
 * @date 2023/4/19 0:16
 */
public class ValidationAnnotationPlugin extends PluginAdapter {

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        addValidationAnnotations(topLevelClass, introspectedTable);
        return true;
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        addValidationAnnotations(topLevelClass, introspectedTable);
        return true;
    }

    /**
     * 为类的字段添加Validation注解
     *
     * @param topLevelClass     生成的类
     * @param introspectedTable 表信息
     */
    private void addValidationAnnotations(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        // 为每个字段添加@Column注解
        for (Field field : topLevelClass.getFields()) {
            // 查找字段对应的列
            final String fieldName = field.getName();
            final Optional<IntrospectedColumn> columnOpt =
                    introspectedTable.getColumn(PluginUtils.getLowerUnderscore(fieldName));
            if (columnOpt.isEmpty()) {
                continue;
            }

            final IntrospectedColumn column = columnOpt.get();

            // 添加@NotBlank或@Length注解（针对字符串类型）
            if (field.getType().getFullyQualifiedName().equals(String.class.getName())) {
                // 即使可为空，也可以添加长度验证
                if (column.getLength() > 0) {
                    topLevelClass.addImportedType("jakarta.validation.constraints.Size");
                    field.addAnnotation("@Size(max = " + column.getLength() + ", message = \"" + fieldName + "'s length cannot " +
                                        "exceed " + column.getLength() + "\")");
                }
                if (!column.isNullable()) {
                    topLevelClass.addImportedType("jakarta.validation.constraints.NotBlank");
                    field.addAnnotation("@NotBlank(message = \"" + fieldName + " cannot be blank\")");
                }
            } else {
                // 添加@NotNull注解（针对非空字段）
                if (!column.isNullable()) {
                    topLevelClass.addImportedType("jakarta.validation.constraints.NotNull");
                    field.addAnnotation("@NotNull(message = \"" + fieldName + " cannot be null\")");
                }
            }
        }
    }


    @Override
    public boolean kotlinDataClassGenerated(final KotlinFile kotlinFile, final KotlinType dataClass,
                                            final IntrospectedTable introspectedTable) {
        // Kotlin中通常使用@Size和@NotBlank等注解
        for (KotlinProperty field : dataClass.getConstructorProperties()) {
            final String fieldName = field.getName();
            final Optional<IntrospectedColumn> columnOpt =
                    introspectedTable.getColumn(PluginUtils.getLowerUnderscore(fieldName));
            if (columnOpt.isEmpty()) {
                continue;
            }

            final IntrospectedColumn column = columnOpt.get();

            if (field.getDataType().get().equalsIgnoreCase(String.class.getName())) {
                // 添加@Size注解（针对字符串长度限制）
                if (field.getDataType().get().equals(String.class.getName()) && column.getLength() > 0) {
                    kotlinFile.addImport("jakarta.validation.constraints.Size");
                    field.addAnnotation("@field:Size(max = " + column.getLength() + ", message = " + fieldName + "\"'s length " +
                                        "cannot exceed " + column.getLength() + "\")");
                }
                // 添加@NotBlank注解（针对非空字符串字段）
                if (!column.isNullable()) {
                    kotlinFile.addImport("jakarta.validation.constraints.NotBlank");
                    field.addAnnotation("@field:NotBlank(message = \"" + fieldName + " cannot be blank\")");
                }
            } else {
                // 添加@NotNull注解（针对非空字段）
                if (!column.isNullable()) {
                    kotlinFile.addImport("jakarta.validation.constraints.NotNull");
                    field.addAnnotation("@field:NotNull(message = \"" + fieldName + " cannot be null\")");
                }
            }
        }
        return true;
    }
}