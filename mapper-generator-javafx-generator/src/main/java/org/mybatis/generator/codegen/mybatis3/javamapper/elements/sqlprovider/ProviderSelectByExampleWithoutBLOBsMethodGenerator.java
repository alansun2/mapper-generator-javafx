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
package org.mybatis.generator.codegen.mybatis3.javamapper.elements.sqlprovider;

import static org.mybatis.generator.codegen.mybatis3.MyBatis3FormattingUtilities.getSelectListPhrase;
import static org.mybatis.generator.internal.util.StringUtility.escapeStringForJava;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.JavaVisibility;
import org.mybatis.generator.api.dom.java.Method;
import org.mybatis.generator.api.dom.java.Parameter;
import org.mybatis.generator.api.dom.java.TopLevelClass;

public class ProviderSelectByExampleWithoutBLOBsMethodGenerator extends AbstractJavaProviderMethodGenerator {

    public ProviderSelectByExampleWithoutBLOBsMethodGenerator(boolean useLegacyBuilder) {
        super(useLegacyBuilder);
    }

    @Override
    public void addClassElements(TopLevelClass topLevelClass) {
        Set<String> staticImports = new TreeSet<>();
        Set<FullyQualifiedJavaType> importedTypes = new TreeSet<>();

        if (useLegacyBuilder) {
            staticImports.add("org.apache.ibatis.jdbc.SqlBuilder.BEGIN");
            staticImports.add("org.apache.ibatis.jdbc.SqlBuilder.SELECT");
            staticImports.add("org.apache.ibatis.jdbc.SqlBuilder.SELECT_DISTINCT");
            staticImports.add("org.apache.ibatis.jdbc.SqlBuilder.FROM");
            staticImports.add("org.apache.ibatis.jdbc.SqlBuilder.ORDER_BY");
            staticImports.add("org.apache.ibatis.jdbc.SqlBuilder.SQL");
        } else {
            importedTypes.add(NEW_BUILDER_IMPORT);
        }

        FullyQualifiedJavaType fqjt = new FullyQualifiedJavaType(introspectedTable.getExampleType());
        importedTypes.add(fqjt);

        Method method = new Method(getMethodName());
        method.setVisibility(JavaVisibility.PUBLIC);
        method.setReturnType(FullyQualifiedJavaType.getStringInstance());
        method.addParameter(new Parameter(fqjt, "example"));
        
        context.getCommentGenerator().addGeneralMethodComment(method,
                introspectedTable);
        
        if (useLegacyBuilder) {
            method.addBodyLine("BEGIN();");
        } else {
            method.addBodyLine("SQL sql = new SQL();");
        }

        boolean distinctCheck = true;
        for (IntrospectedColumn introspectedColumn : getColumns()) {
            if (distinctCheck) {
                method.addBodyLine("if (example != null && example.isDistinct()) {");
                method.addBodyLine(String.format("%sSELECT_DISTINCT(\"%s\");",
                        builderPrefix,
                        escapeStringForJava(getSelectListPhrase(introspectedColumn))));
                method.addBodyLine("} else {");
                method.addBodyLine(String.format("%sSELECT(\"%s\");",
                        builderPrefix,
                        escapeStringForJava(getSelectListPhrase(introspectedColumn))));
                method.addBodyLine("}");
            } else {
                method.addBodyLine(String.format("%sSELECT(\"%s\");",
                        builderPrefix,
                        escapeStringForJava(getSelectListPhrase(introspectedColumn))));
            }

            distinctCheck = false;
        }

        method.addBodyLine(String.format("%sFROM(\"%s\");",
                builderPrefix,
                escapeStringForJava(introspectedTable.getAliasedFullyQualifiedTableNameAtRuntime())));
        if (useLegacyBuilder) {
            method.addBodyLine("applyWhere(example, false);");
        } else {
            method.addBodyLine("applyWhere(sql, example, false);");
        }

        method.addBodyLine("");
        method.addBodyLine("if (example != null && example.getOrderByClause() != null) {");
        method.addBodyLine(String.format("%sORDER_BY(example.getOrderByClause());", builderPrefix));
        method.addBodyLine("}");

        method.addBodyLine("");
        if (useLegacyBuilder) {
            method.addBodyLine("return SQL();");
        } else {
            method.addBodyLine("return sql.toString();");
        }

        if (callPlugins(method, topLevelClass)) {
            topLevelClass.addStaticImports(staticImports);
            topLevelClass.addImportedTypes(importedTypes);
            topLevelClass.addMethod(method);
        }
    }

    public List<IntrospectedColumn> getColumns() {
        return introspectedTable.getNonBLOBColumns();
    }

    public String getMethodName() {
        return introspectedTable.getSelectByExampleStatementId();
    }

    public boolean callPlugins(Method method, TopLevelClass topLevelClass) {
        return context.getPlugins().providerSelectByExampleWithoutBLOBsMethodGenerated(method, topLevelClass,
                introspectedTable);
    }
}
