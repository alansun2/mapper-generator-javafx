package com.alan344.mybatis;

import com.alan344.constants.BaseConstants;
import com.alan344.mybatis.mapper.SelectListMethodGenerator;
import com.alan344.mybatis.xml.ListXMLMapperGenerator;
import org.mybatis.generator.api.CommentGenerator;
import org.mybatis.generator.api.dom.java.CompilationUnit;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.Interface;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.codegen.AbstractJavaClientGenerator;
import org.mybatis.generator.codegen.AbstractXmlGenerator;
import org.mybatis.generator.codegen.mybatis3.javamapper.elements.AbstractJavaMapperMethodGenerator;
import org.mybatis.generator.config.PropertyRegistry;

import java.util.ArrayList;
import java.util.List;

import static org.mybatis.generator.internal.util.StringUtility.stringHasValue;
import static org.mybatis.generator.internal.util.messages.Messages.getString;

/**
 * @author AlanSun
 * @since 2025/12/17 11:15
 **/
public class MybatisFlexJavaClientGenerator extends AbstractJavaClientGenerator {

    public MybatisFlexJavaClientGenerator(String project) {
        this(project, true);
    }

    public MybatisFlexJavaClientGenerator(String project, boolean requiresMatchedXMLGenerator) {
        super(project, requiresMatchedXMLGenerator);
    }

    @Override
    public List<CompilationUnit> getCompilationUnits() {
        progressCallback.startTask(getString("Progress.17", introspectedTable.getFullyQualifiedTable().toString()));
        CommentGenerator commentGenerator = context.getCommentGenerator();

        FullyQualifiedJavaType type = new FullyQualifiedJavaType(introspectedTable.getMyBatis3JavaMapperType());
        Interface interfaze = new Interface(type);
        interfaze.setVisibility(JavaVisibility.PUBLIC);
        interfaze.addAnnotation("@Mapper");
        interfaze.addImportedType(new FullyQualifiedJavaType("org.apache.ibatis.annotations.Mapper"));

        String rootInterface = introspectedTable.getTableConfigurationProperty(PropertyRegistry.ANY_ROOT_INTERFACE);
        if (!stringHasValue(rootInterface)) {
            rootInterface = context.getJavaClientGeneratorConfiguration().getProperty(PropertyRegistry.ANY_ROOT_INTERFACE);
        }

        if (stringHasValue(rootInterface)) {
            rootInterface = rootInterface + "<" + introspectedTable.getBaseRecordType() + ">";
            FullyQualifiedJavaType fqjt = new FullyQualifiedJavaType(rootInterface);
            interfaze.addSuperInterface(fqjt);
            interfaze.addImportedType(fqjt);
        }

        this.addSelectListMethod(interfaze);

        List<CompilationUnit> answer = new ArrayList<>();
        if (context.getPlugins().clientGenerated(interfaze, introspectedTable)) {
            answer.add(interfaze);
        }

        commentGenerator.addInterfaceComment(interfaze, introspectedTable);
        return answer;
    }

    @Override
    public AbstractXmlGenerator getMatchedXMLGenerator() {
        return new ListXMLMapperGenerator();
    }


    @Override
    public boolean requiresXMLGenerator() {
        return BaseConstants.currentConfig.isXmlEnable();
    }

    protected void addSelectListMethod(Interface interfaze) {
        AbstractJavaMapperMethodGenerator methodGenerator = new SelectListMethodGenerator();
        initializeAndExecuteGenerator(methodGenerator, interfaze);
    }

    protected void initializeAndExecuteGenerator(AbstractJavaMapperMethodGenerator methodGenerator,
                                                 Interface interfaze) {
        methodGenerator.setContext(context);
        methodGenerator.setIntrospectedTable(introspectedTable);
        methodGenerator.setProgressCallback(progressCallback);
        methodGenerator.setWarnings(warnings);
        methodGenerator.addInterfaceElements(interfaze);
    }
}
