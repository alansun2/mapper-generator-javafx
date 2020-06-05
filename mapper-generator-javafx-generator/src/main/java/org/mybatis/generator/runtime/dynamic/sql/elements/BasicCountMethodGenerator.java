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
package org.mybatis.generator.runtime.dynamic.sql.elements;

import java.util.HashSet;
import java.util.Set;

import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.Interface;
import org.mybatis.generator.api.dom.java.Method;
import org.mybatis.generator.api.dom.java.Parameter;

public class BasicCountMethodGenerator extends AbstractMethodGenerator {
    
    private BasicCountMethodGenerator(Builder builder) {
        super(builder);
    }

    @Override
    public MethodAndImports generateMethodAndImports() {
        if (!introspectedTable.getRules().generateCountByExample()) {
            return null;
        }
        
        Set<FullyQualifiedJavaType> imports = new HashSet<>();
        
        FullyQualifiedJavaType parameterType =
                new FullyQualifiedJavaType(
                        "org.mybatis.dynamic.sql.select.render.SelectStatementProvider");
        FullyQualifiedJavaType adapter =
                new FullyQualifiedJavaType("org.mybatis.dynamic.sql.util.SqlProviderAdapter");
        FullyQualifiedJavaType annotation =
                new FullyQualifiedJavaType("org.apache.ibatis.annotations.SelectProvider");
        
        imports.add(parameterType);
        imports.add(adapter);
        imports.add(annotation);
        
        Method method = new Method("count");
        method.setAbstract(true);
        method.setReturnType(new FullyQualifiedJavaType("long"));
        method.addParameter(new Parameter(parameterType, "selectStatement"));
        context.getCommentGenerator().addGeneralMethodAnnotation(method, introspectedTable, imports);
        method.addAnnotation("@SelectProvider(type=SqlProviderAdapter.class, method=\"select\")");

        return MethodAndImports.withMethod(method)
                .withImports(imports)
                .build();
    }
    
    @Override
    public boolean callPlugins(Method method, Interface interfaze) {
        return context.getPlugins().clientBasicCountMethodGenerated(method, interfaze, introspectedTable);
    }
    
    public static class Builder extends BaseBuilder<Builder, BasicCountMethodGenerator> {
        @Override
        public Builder getThis() {
            return this;
        }
        
        @Override
        public BasicCountMethodGenerator build() {
            return new BasicCountMethodGenerator(this);
        }
    }
}
