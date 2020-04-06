/**
 *    Copyright 2006-2019 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package org.mybatis.generator.codegen.mybatis3.xmlmapper.elements;

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.dom.OutputUtilities;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.xml.Attribute;
import org.mybatis.generator.api.dom.xml.TextElement;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.codegen.mybatis3.ListUtilities;
import org.mybatis.generator.codegen.mybatis3.MyBatis3FormattingUtilities;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jeff Butler
 *
 */
public class InsertBatchElementGenerator extends AbstractXmlElementGenerator {


    public InsertBatchElementGenerator() {
        super();
    }

    @Override
    public void addElements(XmlElement parentElement) {
        XmlElement answer = new XmlElement("insert"); //$NON-NLS-1$

        answer.addAttribute(new Attribute(
                "id", introspectedTable.getInsertBatchStatementId())); //$NON-NLS-1$

        answer.addAttribute(new Attribute("parameterType", //$NON-NLS-1$
                FullyQualifiedJavaType.getNewListInstance().getFullyQualifiedName()));

        context.getCommentGenerator().addComment(answer);

        this.generateKey(introspectedTable, answer);

        answer.addElement(new TextElement("insert into " + introspectedTable.getFullyQualifiedTableNameAtRuntime()));
        answer.addElement(new TextElement(" ("));

        StringBuilder insertClause = new StringBuilder();
        StringBuilder valuesClause = new StringBuilder();
        List<String> valuesClauses = new ArrayList<String>();
        List<IntrospectedColumn> columns = ListUtilities.removeIdentityAndGeneratedAlwaysColumns(introspectedTable.getAllColumns());
        for (int i = 0; i < columns.size(); i++) {
            IntrospectedColumn introspectedColumn = columns.get(i);

            insertClause.setLength(0);
            valuesClause.setLength(0);
            insertClause.append(MyBatis3FormattingUtilities.getEscapedColumnName(introspectedColumn));
            valuesClause.append(MyBatis3FormattingUtilities.getParameterClause(introspectedColumn, "item."));
            if (i + 1 < columns.size()) {
                insertClause.append(", "); //$NON-NLS-1$
                valuesClause.append(", "); //$NON-NLS-1$
            }

            answer.addElement(new TextElement(insertClause.toString()));
            OutputUtilities.xmlIndent(insertClause, 1);
            valuesClauses.add(valuesClause.toString());
            OutputUtilities.xmlIndent(valuesClause, 3);
        }

        answer.addElement(new TextElement(")"));
        OutputUtilities.xmlIndent(valuesClause, 1);
        answer.addElement(new TextElement("values"));
        OutputUtilities.xmlIndent(valuesClause, 1);
        answer.addElement(new TextElement("<foreach collection=\"list\" item=\"item\" separator=\",\">"));
        answer.addElement(new TextElement("("));

        for (String clause : valuesClauses) {
            answer.addElement(new TextElement(clause));
        }

        answer.addElement(new TextElement(")"));
        answer.addElement(new TextElement("</foreach>"));

        if (context.getPlugins().sqlMapInsertElementGenerated(answer, introspectedTable)) {
            parentElement.addElement(answer);
        }
    }
}
