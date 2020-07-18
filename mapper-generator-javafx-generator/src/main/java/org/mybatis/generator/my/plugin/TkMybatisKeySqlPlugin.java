package org.mybatis.generator.my.plugin;

import com.alan344happyframework.util.StringUtils;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.config.GeneratedKey;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2020/7/15 8:46
 * <p>
 * 用于生成tk.mybatis 的@Keysql
 */
public class TkMybatisKeySqlPlugin extends PluginAdapter {
    /**
     * 用于判断该表的主键是否已生成，加快效率
     */
    private final Set<String> checkContainSet = new HashSet<>();
    /**
     * 用于加快速度
     */
    private final Set<String> checkPrimarySet = new HashSet<>();

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public boolean modelFieldGenerated(Field field, TopLevelClass topLevelClass, IntrospectedColumn introspectedColumn, IntrospectedTable introspectedTable, ModelClassType modelClassType) {

        // 生成 @Id 注解
        if (!checkPrimarySet.contains(introspectedTable.getFullyQualifiedTableNameAtRuntime())) {
            final List<IntrospectedColumn> primaryKeyColumns = introspectedTable.getPrimaryKeyColumns();
            if (!primaryKeyColumns.isEmpty()) {
                final boolean b = primaryKeyColumns.stream().anyMatch(introspectedColumn1 -> introspectedColumn1.getActualColumnName().equals(introspectedColumn.getActualColumnName()));
                if(b){
                    field.addAnnotation("@Id");
                    topLevelClass.addImportedType("javax.persistence.Id");
                }
            }
        }

        if (checkContainSet.contains(introspectedTable.getFullyQualifiedTableNameAtRuntime())) {
            return true;
        }

        // 生成主键策略
        final boolean b = this.generateKey(introspectedTable, field, topLevelClass);

        if (b) {
            checkContainSet.add(introspectedTable.getFullyQualifiedTableNameAtRuntime());
        }

        return true;
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.addConstants(topLevelClass, introspectedTable);
        return super.modelBaseRecordClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelPrimaryKeyClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.addConstants(topLevelClass, introspectedTable);
        return super.modelPrimaryKeyClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.addConstants(topLevelClass, introspectedTable);
        return super.modelRecordWithBLOBsClassGenerated(topLevelClass, introspectedTable);
    }

    private void addConstants(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        final String generateColumnConsts = this.properties.getProperty("generateColumnConsts");
        if (StringUtils.isNotEmpty(generateColumnConsts)) {
            for (IntrospectedColumn introspectedColumn : introspectedTable.getAllColumns()) {
                Field field = new Field(introspectedColumn.getActualColumnName().toUpperCase(), FullyQualifiedJavaType.getStringInstance());
                field.setVisibility(JavaVisibility.PUBLIC);
                field.setStatic(true);
                field.setFinal(true);
                field.setInitializationString("\"" + introspectedColumn.getJavaProperty() + "\"");
                context.getCommentGenerator().addClassComment(topLevelClass, introspectedTable);
                topLevelClass.addField(field);
                //增加字段名常量,用于pageHelper
                Field columnField = new Field("DB_" + introspectedColumn.getActualColumnName().toUpperCase(), FullyQualifiedJavaType.getStringInstance());
                columnField.setVisibility(JavaVisibility.PUBLIC);
                columnField.setStatic(true);
                columnField.setFinal(true);
                columnField.setInitializationString("\"" + introspectedColumn.getActualColumnName() + "\"");
                topLevelClass.addField(columnField);
            }
        }
    }

    /**
     * -----
     * 插入时返回生成主键
     *
     * @param introspectedTable introspectedTable
     * @param topLevelClass     topLevelClass
     */
    protected boolean generateKey(IntrospectedTable introspectedTable, Field field, TopLevelClass topLevelClass) {
        GeneratedKey gk = introspectedTable.getGeneratedKey();
        if (gk != null) {
            introspectedTable.getColumn(gk.getColumn()).ifPresent(introspectedColumn -> {
                // if the column is null, then it's a configuration error. The
                // warning has already been reported
                if (gk.isJdbcStandard()) {
                    topLevelClass.addImportedType("tk.mybatis.mapper.annotation.KeySql");
                    field.addAnnotation("@KeySql(useGeneratedKeys = true)");
                } else {
                    field.addJavaDocLine("@KeySql(dialect = IdentityDialect." + gk.getRuntimeSqlStatement().toUpperCase() + ")");
                }
            });
            return true;
        }

        return false;
    }
}
