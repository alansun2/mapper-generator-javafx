package com.alan344.mybatisplugin;

import com.alan344.constants.BaseConstants;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.TopLevelClass;

import java.sql.Types;
import java.util.List;

/**
 * 将 tinyInt(1) 类型转换为 Boolean 类型的插件
 *
 * @author AlanSun
 * @since 2025/12/17
 */
public class TinyIntToBooleanPlugin extends PluginAdapter {

    @Override
    public boolean validate(List<String> warnings) {
        return BaseConstants.currentConfig.getMybatisExportConfig().isTinyInt1ToBoolean();
    }

    @Override
    public void initialized(IntrospectedTable introspectedTable) {
        // 遍历所有列，查找 TINYINT(1) 类型
        List<IntrospectedColumn> columns = introspectedTable.getAllColumns();
        for (IntrospectedColumn column : columns) {
            // 检查是否是 TINYINT 类型且长度为 1
            if (column.getJdbcType() == Types.TINYINT && column.getLength() == 1) {
                // 修改 Java 类型为 Boolean
                column.setFullyQualifiedJavaType(FullyQualifiedJavaType.getBooleanPrimitiveInstance());
            }
        }

        super.initialized(introspectedTable);
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        PluginUtils.tinyInt2Boolean(topLevelClass, introspectedTable);
        return super.modelBaseRecordClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        PluginUtils.tinyInt2Boolean(topLevelClass, introspectedTable);
        return super.modelRecordWithBLOBsClassGenerated(topLevelClass, introspectedTable);
    }
}
