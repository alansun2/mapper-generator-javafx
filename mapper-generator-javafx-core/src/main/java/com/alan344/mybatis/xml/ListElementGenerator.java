package com.alan344.mybatis.xml;

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.dom.xml.Attribute;
import org.mybatis.generator.api.dom.xml.TextElement;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.codegen.mybatis3.xmlmapper.elements.AbstractXmlElementGenerator;

import java.util.ArrayList;
import java.util.List;

import static org.mybatis.generator.codegen.mybatis3.MyBatis3FormattingUtilities.getAliasedEscapedColumnName;

/**
 * @author AlanSun
 * @since 2025/12/17 11:14
 **/
public class ListElementGenerator extends AbstractXmlElementGenerator {

    @Override
    public void addElements(XmlElement parentElement) {
        XmlElement answer = new XmlElement("select");

        answer.addAttribute(new Attribute("id", "selectList"));
        answer.addAttribute(new Attribute("resultType", introspectedTable.getBaseRecordType()));

        context.getCommentGenerator().addComment(answer);

        this.buildSelectList("SELECT ", introspectedTable.getAllColumns()).forEach(answer::addElement);

        StringBuilder sb = new StringBuilder();
        sb.append("FROM ");
        sb.append(introspectedTable.getAliasedFullyQualifiedTableNameAtRuntime());
        answer.addElement(new TextElement(sb.toString()));

        sb.setLength(0);
        sb.append("ORDER BY id DESC");
        answer.addElement(new TextElement(sb.toString()));

        if (context.getPlugins().sqlMapSelectAllElementGenerated(answer, introspectedTable)) {
            parentElement.addElement(answer);
        }
    }

    @Override
    protected List<TextElement> buildSelectList(String initial, List<IntrospectedColumn> columns) {
        List<TextElement> answer = new ArrayList<>();
        StringBuilder sb = new StringBuilder(initial);
        sb.append("\n");
        final int size = columns.size();
        for (int i = 0; i < size; i++) {
            IntrospectedColumn column = columns.get(i);
            sb.append("    ").append(getAliasedEscapedColumnName(column));
            if (i != size - 1) {
                sb.append(",\n");
            }
        }
        // MyBatis3FormattingUtilities.getSelectListPhrase(column)
        if (!sb.isEmpty()) {
            answer.add(new TextElement(sb.toString()));
        }

        return answer;
    }
}
