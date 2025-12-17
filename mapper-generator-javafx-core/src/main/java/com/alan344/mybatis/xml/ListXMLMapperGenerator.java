package com.alan344.mybatis.xml;

import org.mybatis.generator.api.FullyQualifiedTable;
import org.mybatis.generator.api.dom.xml.Attribute;
import org.mybatis.generator.api.dom.xml.Document;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.codegen.AbstractXmlGenerator;
import org.mybatis.generator.codegen.XmlConstants;
import org.mybatis.generator.codegen.mybatis3.xmlmapper.elements.AbstractXmlElementGenerator;

import static org.mybatis.generator.internal.util.messages.Messages.getString;

/**
 * @author AlanSun
 * @since 2025/12/17 11:14
 **/
public class ListXMLMapperGenerator extends AbstractXmlGenerator {

    public ListXMLMapperGenerator() {
        super();
    }

    protected XmlElement getSqlMapElement() {
        FullyQualifiedTable table = introspectedTable.getFullyQualifiedTable();
        progressCallback.startTask(getString("Progress.12", table.toString()));
        XmlElement answer = new XmlElement("mapper");
        String namespace = introspectedTable.getMyBatis3SqlMapNamespace();
        answer.addAttribute(new Attribute("namespace", namespace));

        context.getCommentGenerator().addRootComment(answer);

        addSelectAllElement(answer);

        return answer;
    }

    private void addSelectAllElement(XmlElement parentElement) {
        AbstractXmlElementGenerator elementGenerator = new ListElementGenerator();
        initializeAndExecuteGenerator(elementGenerator, parentElement);
    }

    private void initializeAndExecuteGenerator(AbstractXmlElementGenerator elementGenerator,
                                               XmlElement parentElement) {
        elementGenerator.setContext(context);
        elementGenerator.setIntrospectedTable(introspectedTable);
        elementGenerator.setProgressCallback(progressCallback);
        elementGenerator.setWarnings(warnings);
        elementGenerator.addElements(parentElement);
    }

    @Override
    public Document getDocument() {
        Document document = new Document(XmlConstants.MYBATIS3_MAPPER_PUBLIC_ID, XmlConstants.MYBATIS3_MAPPER_SYSTEM_ID);
        document.setRootElement(getSqlMapElement());

        if (!context.getPlugins().sqlMapDocumentGenerated(document, introspectedTable)) {
            document = null;
        }

        return document;
    }
}
