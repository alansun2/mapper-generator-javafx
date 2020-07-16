package org.mybatis.generator.my.table;

import org.mybatis.generator.api.ProgressCallback;
import org.mybatis.generator.codegen.AbstractJavaClientGenerator;
import org.mybatis.generator.codegen.AbstractJavaGenerator;
import org.mybatis.generator.codegen.mybatis3.IntrospectedTableMyBatis3Impl;
import org.mybatis.generator.my.clientgenerator.TkMybatisJavaClientGenerator;
import org.mybatis.generator.codegen.mybatis3.model.BaseRecordGenerator;

import java.util.List;

/**
 * @author AlanSun
 * @date 2020-06-05
 */
public class IntrospectedTableTkMybatisImpl extends IntrospectedTableMyBatis3Impl {
    public IntrospectedTableTkMybatisImpl() {
        super();
        targetRuntime = TargetRuntime.MYBATIS3_TK;
    }

    @Override
    protected void calculateXmlMapperGenerator(AbstractJavaClientGenerator javaClientGenerator,
                                               List<String> warnings,
                                               ProgressCallback progressCallback) {
        if (javaClientGenerator == null) {
            xmlMapperGenerator = null;
        } else {
            xmlMapperGenerator = javaClientGenerator.getMatchedXMLGenerator();
        }

        initializeAbstractGenerator(xmlMapperGenerator, warnings, progressCallback);
    }

    @Override
    protected AbstractJavaClientGenerator createJavaClientGenerator() {
        if (context.getJavaClientGeneratorConfiguration() == null) {
            return null;
        }

        return new TkMybatisJavaClientGenerator(getClientProject());
    }

    @Override
    protected void calculateJavaModelGenerators(List<String> warnings, ProgressCallback progressCallback) {
        AbstractJavaGenerator javaGenerator = new BaseRecordGenerator(getModelProject());
        initializeAbstractGenerator(javaGenerator, warnings, progressCallback);
        javaGenerators.add(javaGenerator);
    }
}
