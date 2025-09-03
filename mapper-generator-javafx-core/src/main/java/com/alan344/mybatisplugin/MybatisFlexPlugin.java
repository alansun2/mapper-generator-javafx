package com.alan344.mybatisplugin;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.TopLevelClass;

import java.util.List;
import java.util.Optional;

/**
 * @author AlanSun
 * @since 2025/8/15 09:38
 */
public class MybatisFlexPlugin extends PluginAdapter {
    @Override
    public boolean validate(final List<String> warnings) {
        return true;
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        addMybatisFlexAnnotations(topLevelClass, introspectedTable);
        return true;
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        addMybatisFlexAnnotations(topLevelClass, introspectedTable);
        return true;
    }

    /**
     * 为类和字段添加Mybatis Flex注解
     *
     * @param topLevelClass     生成的类
     * @param introspectedTable 表信息
     */
    private void addMybatisFlexAnnotations(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        // 添加 @Table 注解
        String tableName = introspectedTable.getFullyQualifiedTableNameAtRuntime();
        topLevelClass.addImportedType("com.mybatisflex.annotation.Table");
        topLevelClass.addAnnotation("@Table(\"" + tableName + "\")");

        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        final MybatisExportConfig.MybatisOfficialExportConfig mybatisExportConfig = currentConfig.getMybatisExportConfig();
        // 逻辑删除字段
        final String logicDeleteField = mybatisExportConfig.getLogicDeleteField();
        // 乐观锁字段
        final String version = mybatisExportConfig.getVersion();

        // 处理主键字段
        String primaryKey = null;
        final List<IntrospectedColumn> primaryKeyColumns = introspectedTable.getPrimaryKeyColumns();
        if (CollectionUtil.isNotEmpty(primaryKeyColumns)) {
            final IntrospectedColumn primaryColumn = primaryKeyColumns.get(0);
            final Optional<Field> primaryKeyOpt =
                    topLevelClass.getFields().stream().filter(field -> field.getName().equals(primaryColumn.getJavaProperty()))
                            .findFirst();
            if (primaryKeyOpt.isPresent()) {
                final Field field = primaryKeyOpt.get();
                topLevelClass.addImportedType("com.mybatisflex.annotation.Id");
                field.addAnnotation("@Id");
                primaryKey = primaryColumn.getJavaProperty();
            }
        }

        // 为非主键字段添加 @Column 注解
        for (Field field : topLevelClass.getFields()) {
            // 跳过主键字段，因为主键字段已经处理过了
            if (primaryKey != null && primaryKey.equals(field.getName())) {
                continue;
            }

            // 查找对应的列信息
            final Optional<IntrospectedColumn> columnOpt =
                    introspectedTable.getAllColumns().stream()
                            .filter(column -> column.getJavaProperty().equals(field.getName()))
                            .findFirst();

            if (columnOpt.isEmpty()) {
                continue;
            }

            final IntrospectedColumn column = columnOpt.get();
            // 构建@Column注解
            StringBuilder columnAnnotation = new StringBuilder("@Column(");
            columnAnnotation.append("value = \"").append(column.getActualColumnName()).append("\"");

            // 逻辑删除字段
            if (StrUtil.isNotEmpty(logicDeleteField) && logicDeleteField.equals(column.getActualColumnName())) {
                columnAnnotation.append(", logicDelete = true");
            }

            // 乐观锁字段
            if (StrUtil.isNotEmpty(version) && version.equals(column.getActualColumnName())) {
                columnAnnotation.append(", version = true");
            }

            columnAnnotation.append(")");

            field.addAnnotation(columnAnnotation.toString());
        }
        if (!topLevelClass.getFields().isEmpty()) {
            topLevelClass.addImportedType("com.mybatisflex.annotation.Column");
        }
    }
}
