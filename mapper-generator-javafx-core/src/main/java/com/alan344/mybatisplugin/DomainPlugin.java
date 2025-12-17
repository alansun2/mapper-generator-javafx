package com.alan344.mybatisplugin;

import com.alan344.utils.CollectionUtils;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.api.GeneratedXmlFile;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.PluginAdapter;
import org.mybatis.generator.api.dom.java.AbstractJavaType;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.api.dom.java.InnerClass;
import org.mybatis.generator.api.dom.java.Interface;
import org.mybatis.generator.api.dom.java.Method;
import org.mybatis.generator.api.dom.java.Parameter;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.api.dom.java.TypeParameter;
import org.mybatis.generator.api.dom.kotlin.KotlinFile;
import org.mybatis.generator.api.dom.kotlin.KotlinType;
import org.mybatis.generator.api.dom.xml.Attribute;
import org.mybatis.generator.api.dom.xml.Document;
import org.mybatis.generator.api.dom.xml.VisitableElement;
import org.mybatis.generator.api.dom.xml.XmlElement;

import java.io.File;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @since 2023/4/19 0:16
 */
@Slf4j
public class DomainPlugin extends PluginAdapter {

    @Override
    public boolean validate(List<String> warnings) {
        return true;
    }

    @Override
    public boolean clientGenerated(Interface interfaze, IntrospectedTable introspectedTable) {
        this.test(interfaze, introspectedTable);
        return super.clientGenerated(interfaze, introspectedTable);
    }

    @Override
    public boolean modelBaseRecordClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.test(topLevelClass, introspectedTable);
        return super.modelBaseRecordClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelRecordWithBLOBsClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.test(topLevelClass, introspectedTable);
        return super.modelRecordWithBLOBsClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean modelExampleClassGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.test(topLevelClass, introspectedTable);
        return super.modelExampleClassGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean sqlMapGenerated(GeneratedXmlFile sqlMap, IntrospectedTable introspectedTable) {
        String targetProject = PluginUtils.parse(sqlMap.getTargetProject(),
                PluginUtils.getDomainFromRemarks(introspectedTable.getRemarks(), true));
        File xmlFile = new File(targetProject);
        if (!xmlFile.exists() && !xmlFile.mkdirs()) {
            log.error("创建xml文件夹：{} 失败", targetProject);
        }
        try {
            // 替换 xml 文件
            final Field documentField = GeneratedXmlFile.class.getDeclaredField("document");
            documentField.setAccessible(true);
            final Document document = (Document) documentField.get(sqlMap);
            final XmlElement rootElement = document.getRootElement();
            this.parseDocument(rootElement, introspectedTable.getRemarks());

            final Field targetPackage = GeneratedXmlFile.class.getSuperclass().getDeclaredField("targetProject");
            targetPackage.setAccessible(true);
            targetPackage.set(sqlMap, targetProject);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        return super.sqlMapGenerated(sqlMap, introspectedTable);
    }

    private void parseDocument(VisitableElement visitableElement, String remark) {
        if (visitableElement instanceof XmlElement xmlElement) {
            final List<Attribute> attributes = xmlElement.getAttributes();
            for (Attribute attribute : attributes) {
                final String name = attribute.getName();
                final String value = attribute.getValue();
                if (name.contains("type") || name.contains("Type") || name.contains("namespace")) {
                    final String parse = PluginUtils.parse(value, PluginUtils.getDomainFromRemarks(remark, true));
                    final Field value1;
                    try {
                        value1 = Attribute.class.getDeclaredField("value");
                        value1.setAccessible(true);
                        value1.set(attribute, parse);
                    } catch (NoSuchFieldException | IllegalAccessException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            final List<VisitableElement> elements = xmlElement.getElements();
            if (CollectionUtils.isNotEmpty(elements)) {
                elements.forEach(visitableElement1 -> this.parseDocument(visitableElement1, remark));
            }
        }
    }

    @Override
    public boolean providerGenerated(TopLevelClass topLevelClass, IntrospectedTable introspectedTable) {
        this.test(topLevelClass, introspectedTable);
        return super.providerGenerated(topLevelClass, introspectedTable);
    }

    @Override
    public boolean dynamicSqlSupportGenerated(TopLevelClass supportClass, IntrospectedTable introspectedTable) {
        this.test(supportClass, introspectedTable);
        return super.dynamicSqlSupportGenerated(supportClass, introspectedTable);
    }

    @Override
    public boolean dynamicSqlSupportGenerated(KotlinFile kotlinFile, KotlinType outerSupportObject,
                                              KotlinType innerSupportClass, IntrospectedTable introspectedTable) {
        final String remarks = introspectedTable.getRemarks();
        this.kotlin(kotlinFile, remarks);

        final Set<String> imports = kotlinFile.getImports();
        final Set<String> collect = imports.stream().map(s -> PluginUtils.parse(s, PluginUtils.getDomainFromRemarks(remarks,
                true))).collect(Collectors.toSet());

        this.reflect(kotlinFile, "imports", collect);

        return super.dynamicSqlSupportGenerated(kotlinFile, outerSupportObject, innerSupportClass, introspectedTable);
    }

    @Override
    public boolean mapperGenerated(KotlinFile mapperFile, KotlinType mapper, IntrospectedTable introspectedTable) {
        final String remarks = introspectedTable.getRemarks();
        this.kotlin(mapperFile, remarks);

        final Set<String> imports = mapperFile.getImports();
        final Set<String> collect = imports.stream().map(s -> PluginUtils.parse(s, PluginUtils.getDomainFromRemarks(remarks,
                true))).collect(Collectors.toSet());

        this.reflect(mapperFile, "imports", collect);
        return super.mapperGenerated(mapperFile, mapper, introspectedTable);
    }

    @Override
    public boolean kotlinDataClassGenerated(KotlinFile kotlinFile, KotlinType dataClass, IntrospectedTable introspectedTable) {
        this.kotlin(kotlinFile, introspectedTable.getRemarks());
        return super.kotlinDataClassGenerated(kotlinFile, dataClass, introspectedTable);
    }

    private void test(AbstractJavaType abstractJavaType, IntrospectedTable introspectedTable) {
        this.type(abstractJavaType.getType(), introspectedTable);

        final Set<FullyQualifiedJavaType> superInterfaceTypes = abstractJavaType.getSuperInterfaceTypes();
        if (CollectionUtils.isNotEmpty(superInterfaceTypes)) {
            superInterfaceTypes.forEach(type -> this.type(type, introspectedTable));
        }

        if (abstractJavaType instanceof InnerClass interfaze) {
            interfaze.getSuperClass().ifPresent(type -> this.type(type, introspectedTable));
        }

        if (abstractJavaType instanceof TopLevelClass topLevelClass) {
            final Set<FullyQualifiedJavaType> importedTypes = topLevelClass.getImportedTypes();
            importedTypes.forEach(type -> this.type(type, introspectedTable));

            final Set<String> staticImports = topLevelClass.getStaticImports();
            if (CollectionUtils.isNotEmpty(staticImports)) {
                final Set<String> collect = staticImports.stream().map(s -> PluginUtils.parse(s,
                        PluginUtils.getDomainFromRemarks(introspectedTable.getRemarks(), true))).collect(Collectors.toSet());
                this.reflect(topLevelClass, "staticImports", collect);
            }
        }

        if (abstractJavaType instanceof Interface interfaze) {
            final Set<FullyQualifiedJavaType> importedTypes = interfaze.getImportedTypes();
            importedTypes.forEach(type -> this.type(type, introspectedTable));

            final Set<String> staticImports = interfaze.getStaticImports();
            if (CollectionUtils.isNotEmpty(staticImports)) {
                final Set<String> collect = staticImports.stream().map(s -> PluginUtils.parse(s,
                        PluginUtils.getDomainFromRemarks(introspectedTable.getRemarks(), true))).collect(Collectors.toSet());
                this.reflect(abstractJavaType, "staticImports", collect);
            }
        }

        final List<Method> methods = abstractJavaType.getMethods();
        methods.forEach(method -> {
            method.getReturnType().ifPresent(type -> this.type(type, introspectedTable));

            final List<Parameter> parameters = method.getParameters();
            if (CollectionUtils.isNotEmpty(parameters)) {
                parameters.forEach(parameter -> this.type(parameter.getType(), introspectedTable));
            }
            final List<TypeParameter> typeParameters = method.getTypeParameters();
            typeParameters.forEach(typeParameter -> {
                typeParameter.getExtendsTypes().forEach(type -> this.type(type, introspectedTable));
            });
        });
    }

    private void modifyPackageName(FullyQualifiedJavaType type, String name, String remark) {
        try {
            final Field typeField = FullyQualifiedJavaType.class.getDeclaredField(name);
            typeField.setAccessible(true);
            String packageName = typeField.get(type).toString();

            packageName = PluginUtils.parse(packageName, PluginUtils.getDomainFromRemarks(remark, true));
            typeField.set(type, packageName);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    private void kotlin(KotlinFile kotlinFile, String remark) {
        final Optional<String> aPackage = kotlinFile.getPackage();
        aPackage.ifPresent(s -> {
            String packageName = PluginUtils.parse(s, PluginUtils.getDomainFromRemarks(remark, true));
            kotlinFile.setPackage(packageName);
        });
    }

    private void reflect(Object o, String name, Object value) {
        Field targetPackage = null;
        try {
            targetPackage = o.getClass().getDeclaredField(name);
        } catch (NoSuchFieldException ignore) {

        }

        if (null == targetPackage) {
            try {
                targetPackage = o.getClass().getSuperclass().getDeclaredField(name);
            } catch (NoSuchFieldException e) {
                throw new RuntimeException(e);
            }
        }

        targetPackage.setAccessible(true);
        try {
            targetPackage.set(o, value);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    private void type(FullyQualifiedJavaType type1, IntrospectedTable introspectedTable) {
        final List<FullyQualifiedJavaType> typeArguments = type1.getTypeArguments();
        if (CollectionUtils.isNotEmpty(typeArguments)) {
            typeArguments.forEach(type2 -> {
                this.modifyPackageName(type2, "packageName", introspectedTable.getRemarks());
                this.modifyPackageName(type2, "baseQualifiedName", introspectedTable.getRemarks());
            });
        }

        final String remarks = introspectedTable.getRemarks();
        this.modifyPackageName(type1, "packageName", remarks);
        this.modifyPackageName(type1, "baseQualifiedName", remarks);
    }
}
