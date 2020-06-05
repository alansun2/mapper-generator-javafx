package org.mybatis.generator.my;

import java.util.List;

import org.mybatis.generator.api.ProgressCallback;
import org.mybatis.generator.codegen.AbstractJavaClientGenerator;
import org.mybatis.generator.codegen.AbstractJavaGenerator;
import org.mybatis.generator.codegen.mybatis3.IntrospectedTableMyBatis3Impl;
import org.mybatis.generator.runtime.dynamic.sql.DynamicSqlMapperGenerator;
import org.mybatis.generator.runtime.dynamic.sql.DynamicSqlModelGenerator;

/**
 * @author AlanSun
 * @date 2020-06-05
 */
public class IntrospectedTableMyBatis3AlanImpl extends IntrospectedTableMyBatis3Impl {
    public IntrospectedTableMyBatis3AlanImpl() {
        super();
        targetRuntime = TargetRuntime.MYBATIS3_MY;
    }

    @Override
    protected void calculateXmlMapperGenerator(AbstractJavaClientGenerator javaClientGenerator, 
            List<String> warnings,
            ProgressCallback progressCallback) {
        // no XML with dynamic SQL support
        xmlMapperGenerator = null;
    }

    @Override
    protected AbstractJavaClientGenerator createJavaClientGenerator() {
        if (context.getJavaClientGeneratorConfiguration() == null) {
            return null;
        }

        return new DynamicSqlMapperGenerator(getClientProject());
    }

    @Override
    protected void calculateJavaModelGenerators(List<String> warnings, ProgressCallback progressCallback) {

        AbstractJavaGenerator javaGenerator = new DynamicSqlModelGenerator(getModelProject());
        initializeAbstractGenerator(javaGenerator, warnings, progressCallback);
        javaGenerators.add(javaGenerator);
    }

    @Override
    public boolean requiresXMLGenerator() {
        return false;
    }
}
