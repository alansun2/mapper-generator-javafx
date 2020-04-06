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

import static org.mybatis.generator.internal.util.StringUtility.stringHasValue;

import org.mybatis.generator.api.dom.xml.Attribute;
import org.mybatis.generator.api.dom.xml.TextElement;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.codegen.mybatis3.MyBatis3FormattingUtilities;

public class SelectByExampleWithBLOBsElementGenerator extends
        AbstractXmlElementGenerator {

    public SelectByExampleWithBLOBsElementGenerator() {
        super();
    }

    @Override
    public void addElements(XmlElement parentElement) {
        String fqjt = introspectedTable.getExampleType();

        XmlElement answer = new XmlElement("select"); //$NON-NLS-1$
        answer
                .addAttribute(new Attribute(
                        "id", introspectedTable.getSelectByExampleWithBLOBsStatementId())); //$NON-NLS-1$
        answer.addAttribute(new Attribute(
                "resultMap", introspectedTable.getResultMapWithBLOBsId())); //$NON-NLS-1$
        answer.addAttribute(new Attribute("parameterType", fqjt)); //$NON-NLS-1$

        context.getCommentGenerator().addComment(answer);

        answer.addElement(new TextElement("select")); //$NON-NLS-1$
        XmlElement ifElement = new XmlElement("if"); //$NON-NLS-1$
        ifElement.addAttribute(new Attribute("test", "distinct")); //$NON-NLS-1$ //$NON-NLS-2$
        ifElement.addElement(new TextElement("distinct")); //$NON-NLS-1$
        answer.addElement(ifElement);

        StringBuilder sb = new StringBuilder();
        if (stringHasValue(introspectedTable
                .getSelectByExampleQueryId())) {
            sb.append('\'');
            sb.append(introspectedTable.getSelectByExampleQueryId());
            sb.append("' as QUERYID,"); //$NON-NLS-1$
            answer.addElement(new TextElement(sb.toString()));
        }

        answer.addElement(getBaseColumnListElement());
        answer.addElement(new TextElement(",")); //$NON-NLS-1$
        answer.addElement(getBlobColumnListElement());

        sb.setLength(0);
        sb.append("from "); //$NON-NLS-1$
        sb.append(introspectedTable.getAliasedFullyQualifiedTableNameAtRuntime());
        answer.addElement(new TextElement(sb.toString()));
        answer.addElement(getExampleIncludeElement());

        ifElement = new XmlElement("if"); //$NON-NLS-1$
        ifElement.addAttribute(new Attribute("test", "orderByClause != null")); //$NON-NLS-1$ //$NON-NLS-2$
        ifElement.addElement(new TextElement("order by ${orderByClause}")); //$NON-NLS-1$
        answer.addElement(ifElement);

        XmlElement ifElement1 = new XmlElement("if"); //$NON-NLS-1$
        ifElement1.addAttribute(new Attribute("test", "limit != null")); //$NON-NLS-1$ //$NON-NLS-2$
        XmlElement ifElement2 = new XmlElement("if"); //$NON-NLS-1$
        ifElement2.addAttribute(new Attribute("test", "offset != null")); //$NON-NLS-1$ //$NON-NLS-2$
        ifElement2.addElement(new TextElement("limit ${offset}, ${limit}"));
        ifElement1.addElement(0, ifElement2); //$NON-NLS-1$
        ifElement2 = new XmlElement("if"); //$NON-NLS-1$
        ifElement2.addAttribute(new Attribute("test", "offset == null")); //$NON-NLS-1$ //$NON-NLS-2$
        ifElement2.addElement(new TextElement("limit ${limit}"));
        ifElement1.addElement(1, ifElement2); //$NON-NLS-1$
        answer.addElement(ifElement1);

        if (context.getPlugins().sqlMapSelectByExampleWithBLOBsElementGenerated(answer, introspectedTable)) {
            parentElement.addElement(answer);
        }
    }
}
