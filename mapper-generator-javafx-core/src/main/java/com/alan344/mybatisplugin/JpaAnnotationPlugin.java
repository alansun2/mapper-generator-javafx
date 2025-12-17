package com.alan344.mybatisplugin;

import cn.hutool.core.collection.CollectionUtil;
import com.alan344.constants.BaseConstants;
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
 * 为Model类添加JPA注解的插件
 * 添加@Entity、@Table和@Column注解
 *
 * @author AlanSun
 * @since 2023/4/19 0:16
 */
public class JpaAnnotationPlugin extends PluginAdapter {

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        addJpaAnnotations(topLevelClass, introspectedTable);
        return true;
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        addJpaAnnotations(topLevelClass, introspectedTable);
        return true;
    }

    /**
     * 为类和字段添加JPA注解
     *
     * @param topLevelClass     生成的类
     * @param introspectedTable 表信息
     */
    private void addJpaAnnotations(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        String packagePrefix = PluginUtils.getPackagePrefix();
        
        // 添加@Entity注解
        topLevelClass.addAnnotation("@Entity");
        topLevelClass.addImportedType(packagePrefix + ".persistence.Entity");

        // 添加@Table注解
        String tableName = introspectedTable.getFullyQualifiedTable().getIntrospectedTableName();
        topLevelClass.addAnnotation("@Table(name = \"" + tableName + "\")");
        topLevelClass.addImportedType(packagePrefix + ".persistence.Table");

        String primaryKey = null;
        final List<IntrospectedColumn> primaryKeyColumns = introspectedTable.getPrimaryKeyColumns();
        if (CollectionUtil.isNotEmpty(primaryKeyColumns)) {
            final IntrospectedColumn primaryColumn = primaryKeyColumns.get(0);
            final Optional<Field> primaryKeyOpt =
                    topLevelClass.getFields().stream().filter(field -> field.getName().equals(primaryColumn.getJavaProperty()))
                            .findFirst();
            if (primaryKeyOpt.isPresent()) {
                final Field field = primaryKeyOpt.get();
                field.addAnnotation("@Id");
                topLevelClass.addImportedType(packagePrefix + ".persistence.Id");
                primaryKey = primaryColumn.getJavaProperty();
            }
        }

        for (Field field : topLevelClass.getFields()) {
            if (primaryKey != null && primaryKey.equals(field.getName())) {
                continue;
            }
            final Optional<IntrospectedColumn> columnOpt =
                    introspectedTable.getColumn(PluginUtils.getLowerUnderscore(field.getName()));
            if (columnOpt.isEmpty()) {
                continue;
            }

            final IntrospectedColumn column = columnOpt.get();
            // 构建@Column注解
            StringBuilder columnAnnotation = new StringBuilder();
            columnAnnotation.append("@Column(name = \"").append(column.getActualColumnName()).append("\"");

            // 添加nullable属性
            if (!column.isNullable()) {
                columnAnnotation.append(", nullable = false");
            }

            // 添加length属性（仅对字符串类型）
            int jdbcType = column.getJdbcType();
            if (jdbcType == java.sql.Types.CHAR || jdbcType == java.sql.Types.VARCHAR ||
                jdbcType == java.sql.Types.LONGVARCHAR || jdbcType == java.sql.Types.CLOB) {
                columnAnnotation.append(", length = ").append(column.getLength());
            }

            columnAnnotation.append(")");

            field.addAnnotation(columnAnnotation.toString());
        }
        topLevelClass.addImportedType(packagePrefix + ".persistence.Column");
    }

    @Override
    public boolean kotlinDataClassGenerated(final KotlinFile kotlinFile, final KotlinType dataClass,
                                            final IntrospectedTable introspectedTable) {
        String packagePrefix = PluginUtils.getPackagePrefix();
        
        // 添加@Entity注解
        dataClass.addAnnotation("@Entity");
        kotlinFile.addImport(packagePrefix + ".persistence.Entity");
        // 添加@Table注解
        String tableName = introspectedTable.getFullyQualifiedTable().getIntrospectedTableName();
        dataClass.addAnnotation("@Table(name = \"" + tableName + "\")");
        kotlinFile.addImport(packagePrefix + ".persistence.Table");

        String primaryKey = null;
        final List<IntrospectedColumn> primaryKeyColumns = introspectedTable.getPrimaryKeyColumns();
        if (CollectionUtil.isNotEmpty(primaryKeyColumns)) {
            final IntrospectedColumn primaryColumn = primaryKeyColumns.get(0);
            final Optional<KotlinProperty> primaryKeyOpt =
                    dataClass.getConstructorProperties().stream().filter(field -> field.getName().equals(primaryColumn.getJavaProperty()))
                            .findFirst();
            if (primaryKeyOpt.isPresent()) {
                final KotlinProperty field = primaryKeyOpt.get();
                field.addAnnotation("@field:Id");
                kotlinFile.addImport(packagePrefix + ".persistence.Id");
                primaryKey = primaryColumn.getJavaProperty();
            }
        }

        // 为每个字段添加@Column注解
        for (KotlinProperty constructorProperty : dataClass.getConstructorProperties()) {
            if (primaryKey != null && primaryKey.equals(constructorProperty.getName())) {
                continue;
            }
            final Optional<IntrospectedColumn> columnOpt =
                    introspectedTable.getColumn(PluginUtils.getLowerUnderscore(constructorProperty.getName()));
            if (columnOpt.isEmpty()) {
                continue;
            }

            final IntrospectedColumn column = columnOpt.get();
            // 构建@Column注解
            StringBuilder columnAnnotation = new StringBuilder();
            columnAnnotation.append("@field:Column(name = \"").append(column.getActualColumnName()).append("\"");

            // 添加nullable属性
            if (!column.isNullable()) {
                columnAnnotation.append(", nullable = false");
            }

            // 添加length属性（仅对字符串类型）
            int jdbcType = column.getJdbcType();
            if (jdbcType == java.sql.Types.CHAR || jdbcType == java.sql.Types.VARCHAR ||
                jdbcType == java.sql.Types.LONGVARCHAR || jdbcType == java.sql.Types.CLOB) {
                columnAnnotation.append(", length = ").append(column.getLength());
            }

            columnAnnotation.append(")");

            constructorProperty.addAnnotation(columnAnnotation.toString());
        }
        kotlinFile.addImport(packagePrefix + ".persistence.Column");
        return true;
    }
}