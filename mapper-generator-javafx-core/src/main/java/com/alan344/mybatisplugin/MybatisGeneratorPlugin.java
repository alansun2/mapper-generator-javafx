package com.alan344.mybatisplugin;

import com.alan344.constants.BaseConstants;
import org.mybatis.generator.api.GeneratedXmlFile;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Interface;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.api.dom.kotlin.KotlinFile;
import org.mybatis.generator.api.dom.kotlin.KotlinType;

import java.util.List;

/**
 * @author AlanSun
 * @since 2023/3/27 0:12
 */
public class MybatisGeneratorPlugin extends PluginAdapter {
    private final boolean enableModel, enableClient, enableXml;

    public MybatisGeneratorPlugin() {
        this.enableModel = BaseConstants.currentConfig.isModelEnable();
        this.enableClient = BaseConstants.currentConfig.isMapperEnable();
        this.enableXml = BaseConstants.currentConfig.isXmlEnable();
    }

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public void initialized(IntrospectedTable introspectedTable) {
        super.initialized(introspectedTable);
    }

    @Override
    public boolean clientGenerated(Interface interfaze, IntrospectedTable introspectedTable) {
        if (!enableClient) {
            return false;
        }
        return super.clientGenerated(interfaze, introspectedTable);
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        if (!enableModel) {
            return false;
        }
        return super.modelBaseRecordClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        if (!enableModel) {
            return false;
        }
        return super.modelRecordWithBLOBsClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelExampleClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        if (!enableModel) {
            return false;
        }
        return super.modelExampleClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean sqlMapGenerated(GeneratedXmlFile sqlMap, IntrospectedTable introspectedTable) {
        if (!enableXml) {
            return false;
        }
        return super.sqlMapGenerated(sqlMap, introspectedTable);
    }

    @Override
    public boolean providerGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        if (!enableClient) {
            return false;
        }
        return super.providerGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean dynamicSqlSupportGenerated(TopLevelClass supportClass, IntrospectedTable introspectedTable) {
        if (!enableClient) {
            return false;
        }
        return super.dynamicSqlSupportGenerated(supportClass, introspectedTable);
    }

    @Override
    public boolean dynamicSqlSupportGenerated(KotlinFile kotlinFile, KotlinType outerSupportObject, KotlinType innerSupportClass, IntrospectedTable introspectedTable) {
        if (!enableClient) {
            return false;
        }
        return super.dynamicSqlSupportGenerated(kotlinFile, outerSupportObject, innerSupportClass, introspectedTable);
    }

    @Override
    public boolean mapperGenerated(KotlinFile mapperFile, KotlinType mapper, IntrospectedTable introspectedTable) {
        if (!enableClient) {
            return false;
        }
        return super.mapperGenerated(mapperFile, mapper, introspectedTable);
    }

    @Override
    public boolean kotlinDataClassGenerated(KotlinFile kotlinFile, KotlinType dataClass, IntrospectedTable introspectedTable) {
        if (!enableModel) {
            return false;
        }
        return super.kotlinDataClassGenerated(kotlinFile, dataClass, introspectedTable);
    }
}
