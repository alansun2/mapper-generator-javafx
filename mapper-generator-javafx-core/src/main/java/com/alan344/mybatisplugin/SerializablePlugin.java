package com.alan344.mybatisplugin;

import cn.hutool.core.util.StrUtil;
import com.alan344.constants.TablePropertyConstants;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.IntrospectedTable.TargetRuntime;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.Field;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.api.dom.kotlin.KotlinFile;
import org.mybatis.generator.api.dom.kotlin.KotlinType;

import java.util.List;
import java.util.Properties;

public class SerializablePlugin extends PluginAdapter {

    private final FullyQualifiedJavaType serializable;
    private final FullyQualifiedJavaType gwtSerializable;
    private boolean addGWTInterface;
    private boolean suppressJavaInterface;

    public SerializablePlugin() {
        super();
        serializable = new FullyQualifiedJavaType("java.io.Serializable");
        gwtSerializable = new FullyQualifiedJavaType("com.google.gwt.user.client.rpc.IsSerializable");
    }

    @Override
    public boolean validate(List<String> warnings) {
        // this plugin is always valid
        return true;
    }

    @Override
    public void setProperties(Properties properties) {
        super.setProperties(properties);
        addGWTInterface = Boolean.parseBoolean(properties.getProperty("addGWTInterface"));
        suppressJavaInterface = Boolean.parseBoolean(properties.getProperty("suppressJavaInterface"));
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        makeSerializable(topLevelClass, introspectedTable);
        return true;
    }

    @Override
    public boolean modelPrimaryKeyClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        makeSerializable(topLevelClass, introspectedTable);
        return true;
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        makeSerializable(topLevelClass, introspectedTable);
        return true;
    }

    protected void makeSerializable(TopLevelClass topLevelClass,
                                    IntrospectedTable introspectedTable) {
        final String jdkSerializable = introspectedTable.getTableConfigurationProperty(TablePropertyConstants.JDK_SERIALIZABLE);
        if (StrUtil.isEmpty(jdkSerializable) || !Boolean.parseBoolean(jdkSerializable)) {
            return;
        }
        if (addGWTInterface) {
            topLevelClass.addImportedType(gwtSerializable);
            topLevelClass.addSuperInterface(gwtSerializable);
        }

        if (!suppressJavaInterface) {
            topLevelClass.addImportedType(serializable);
            topLevelClass.addSuperInterface(serializable);

            Field field = new Field("serialVersionUID", new FullyQualifiedJavaType("long"));
            field.setFinal(true);
            field.setInitializationString("1L");
            field.setStatic(true);
            field.setVisibility(JavaVisibility.PRIVATE);

            if (introspectedTable.getTargetRuntime() == TargetRuntime.MYBATIS3_DSQL) {
                context.getCommentGenerator().addFieldAnnotation(field, introspectedTable,
                        topLevelClass.getImportedTypes());
            } else {
                context.getCommentGenerator().addFieldComment(field, introspectedTable);
            }

            topLevelClass.addField(field);
        }
    }

    @Override
    public boolean kotlinDataClassGenerated(KotlinFile kotlinFile, KotlinType dataClass, IntrospectedTable introspectedTable) {
        final String jdkSerializable = introspectedTable.getTableConfigurationProperty(TablePropertyConstants.JDK_SERIALIZABLE);
        if (StrUtil.isEmpty(jdkSerializable) || !Boolean.parseBoolean(jdkSerializable)) {
            return true;
        }
        kotlinFile.addImport("java.io.Serializable");
        dataClass.addSuperType("Serializable");
        return true;
    }
}
