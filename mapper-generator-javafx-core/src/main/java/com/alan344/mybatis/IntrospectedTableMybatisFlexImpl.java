package com.alan344.mybatis;

import org.mybatis.generator.api.ProgressCallback;
import org.mybatis.generator.codegen.AbstractJavaClientGenerator;
import org.mybatis.generator.codegen.AbstractJavaGenerator;
import org.mybatis.generator.codegen.mybatis3.IntrospectedTableMyBatis3Impl;
import org.mybatis.generator.codegen.mybatis3.model.BaseRecordGenerator;

import java.util.List;

/**
 * @author AlanSun
 * @since 2025/8/3 15:28
 */
public class IntrospectedTableMybatisFlexImpl extends IntrospectedTableMyBatis3Impl {
    public IntrospectedTableMybatisFlexImpl() {
        super();
    }

    @Override
    protected AbstractJavaClientGenerator createJavaClientGenerator() {
        if (context.getJavaClientGeneratorConfiguration() == null) {
            return null;
        }

        return new MybatisFlexJavaClientGenerator(getClientProject());
    }

    @Override
    protected void calculateJavaModelGenerators(List<String> warnings, ProgressCallback progressCallback) {

        AbstractJavaGenerator javaGenerator = new BaseRecordGenerator(getModelProject());
        initializeAbstractGenerator(javaGenerator, warnings, progressCallback);
        javaGenerators.add(javaGenerator);
    }
}
