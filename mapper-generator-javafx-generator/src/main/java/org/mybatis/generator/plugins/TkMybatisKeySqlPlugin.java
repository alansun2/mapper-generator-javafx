package org.mybatis.generator.plugins;

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.config.GeneratedKey;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public boolean modelFieldGenerated(Field field, TopLevelClass topLevelClass, IntrospectedColumn introspectedColumn, IntrospectedTable introspectedTable, ModelClassType modelClassType) {

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
                    field.addJavaDocLine("@KeySql(useGeneratedKeys = true)");
                } else {
                    field.addJavaDocLine("@KeySql(dialect = IdentityDialect." + gk.getRuntimeSqlStatement().toUpperCase() + ")");
                }
            });
            return true;
        }

        return false;
    }
}
