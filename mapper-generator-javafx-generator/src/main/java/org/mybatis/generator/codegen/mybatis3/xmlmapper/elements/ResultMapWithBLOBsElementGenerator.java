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

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.dom.xml.Attribute;
import org.mybatis.generator.api.dom.xml.XmlElement;
import org.mybatis.generator.codegen.mybatis3.MyBatis3FormattingUtilities;

public class ResultMapWithBLOBsElementGenerator extends
        AbstractXmlElementGenerator {

    public ResultMapWithBLOBsElementGenerator() {
        super();
    }

    @Override
    public void addElements(XmlElement parentElement) {
        XmlElement answer = new XmlElement("resultMap");

        answer.addAttribute(new Attribute("id",
                introspectedTable.getResultMapWithBLOBsId()));

        String returnType;
        if (introspectedTable.getRules().generateRecordWithBLOBsClass()) {
            returnType = introspectedTable.getRecordWithBLOBsType();
        } else {
            // table has BLOBs, but no BLOB class - BLOB fields must be
            // in the base class
            returnType = introspectedTable.getBaseRecordType();
        }

        answer.addAttribute(new Attribute("type",
                returnType));

        if (!introspectedTable.isConstructorBased()) {
            answer.addAttribute(new Attribute("extends",
                    introspectedTable.getBaseResultMapId()));
        }

        context.getCommentGenerator().addComment(answer);

        if (introspectedTable.isConstructorBased()) {
            addResultMapConstructorElements(answer);
        } else {
            addResultMapElements(answer);
        }

        if (context.getPlugins()
                .sqlMapResultMapWithBLOBsElementGenerated(answer,
                        introspectedTable)) {
            parentElement.addElement(answer);
        }
    }

    private void addResultMapElements(XmlElement answer) {
        for (IntrospectedColumn introspectedColumn : introspectedTable
                .getBLOBColumns()) {
            XmlElement resultElement = new XmlElement("result");

            resultElement.addAttribute(generateColumnAttribute(introspectedColumn));
            resultElement.addAttribute(new Attribute(
                    "property", introspectedColumn.getJavaProperty()));
            resultElement.addAttribute(new Attribute(
                    "jdbcType", introspectedColumn.getJdbcTypeName()));

            if (stringHasValue(introspectedColumn
                    .getTypeHandler())) {
                resultElement.addAttribute(new Attribute(
                        "typeHandler", introspectedColumn.getTypeHandler()));
            }

            answer.addElement(resultElement);
        }
    }

    private void addResultMapConstructorElements(XmlElement answer) {
        XmlElement constructor = new XmlElement("constructor");
        
        for (IntrospectedColumn introspectedColumn : introspectedTable
                .getPrimaryKeyColumns()) {
            XmlElement resultElement = new XmlElement("idArg");

            resultElement.addAttribute(generateColumnAttribute(introspectedColumn));
            resultElement.addAttribute(new Attribute(
                    "jdbcType", introspectedColumn.getJdbcTypeName()));
            resultElement.addAttribute(new Attribute("javaType",
                    introspectedColumn.getFullyQualifiedJavaType().getFullyQualifiedName()));

            if (stringHasValue(introspectedColumn
                    .getTypeHandler())) {
                resultElement.addAttribute(new Attribute(
                        "typeHandler", introspectedColumn.getTypeHandler()));
            }

            constructor.addElement(resultElement);
        }
        
        for (IntrospectedColumn introspectedColumn : introspectedTable
                .getNonPrimaryKeyColumns()) {
            XmlElement resultElement = new XmlElement("arg");

            resultElement.addAttribute(generateColumnAttribute(introspectedColumn));
            resultElement.addAttribute(new Attribute(
                    "jdbcType", introspectedColumn.getJdbcTypeName()));

            if (introspectedColumn.getFullyQualifiedJavaType().isPrimitive()) {
                // need to use the MyBatis type alias for a primitive byte
                StringBuilder sb = new StringBuilder();
                sb.append('_');
                sb.append(introspectedColumn.getFullyQualifiedJavaType().getShortName());
                resultElement.addAttribute(new Attribute("javaType",
                        sb.toString()));

            } else if ("byte[]".equals(introspectedColumn.getFullyQualifiedJavaType()
                    .getFullyQualifiedName())) {
                // need to use the MyBatis type alias for a primitive byte arry
                resultElement.addAttribute(new Attribute("javaType",
                        "_byte[]"));
            } else {
                resultElement.addAttribute(new Attribute("javaType",
                        introspectedColumn.getFullyQualifiedJavaType().getFullyQualifiedName()));
            }
            
            if (stringHasValue(introspectedColumn
                    .getTypeHandler())) {
                resultElement.addAttribute(new Attribute(
                        "typeHandler", introspectedColumn.getTypeHandler()));
            }

            constructor.addElement(resultElement);
        }

        answer.addElement(constructor);
    }

    private Attribute generateColumnAttribute(IntrospectedColumn introspectedColumn) {
        return new Attribute("column",
                MyBatis3FormattingUtilities.getRenamedColumnNameForResultMap(introspectedColumn));
    }
}
