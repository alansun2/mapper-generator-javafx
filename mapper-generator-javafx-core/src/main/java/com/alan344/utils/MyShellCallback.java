package com.alan344.utils;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.nodeTypes.NodeWithSimpleName;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.config.MergeConstants;
import org.mybatis.generator.exception.ShellException;
import org.mybatis.generator.internal.DefaultShellCallback;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/17 19:43
 * merge 现在不用，有点问题
 */
@Slf4j
public class MyShellCallback extends DefaultShellCallback {
    private final boolean supportMerge;

    public MyShellCallback(boolean overwrite, boolean supportMerge) {
        super(overwrite);
        this.supportMerge = supportMerge;
    }

    @Override
    public File getDirectory(String targetProject, String targetPackage) throws ShellException {
        return super.getDirectory(targetProject, targetPackage);
    }

    @Override
    public boolean isMergeSupported() {
        return supportMerge;
    }

    @Override
    public String mergeJavaFile(String newFileSource, File existingFile, String[] javadocTags, String fileEncoding) {
        try {
            return this.getNewJavaFile(newFileSource, existingFile.getAbsolutePath());
        } catch (FileNotFoundException e) {
            log.error("merge fail", e);
        }
        return newFileSource;
    }

    private String getNewJavaFile(String newFileSource, String existingFileFullPath) throws FileNotFoundException {
        JavaParser javaParser = new JavaParser();
        ParseResult<CompilationUnit> newCompilationUnitParse = javaParser.parse(newFileSource);
        CompilationUnit newCompilationUnit;
        if (newCompilationUnitParse.isSuccessful() && newCompilationUnitParse.getResult().isPresent()) {
            newCompilationUnit = newCompilationUnitParse.getResult().get();
        } else {
            log.error("解析 newFileSource 失败， {}", newCompilationUnitParse.getProblem(0).toString());
            return newFileSource;
        }

        ParseResult<CompilationUnit> existingCompilationUnitParse = javaParser.parse(new File(existingFileFullPath));
        CompilationUnit existingCompilationUnit;
        if (existingCompilationUnitParse.isSuccessful() && existingCompilationUnitParse.getResult().isPresent()) {
            existingCompilationUnit = existingCompilationUnitParse.getResult().get();
        } else {
            log.error("解析 existingFileFullPath 失败， {}", existingCompilationUnitParse.getProblem(0).toString());
            return newFileSource;
        }
        return mergerFile(existingCompilationUnit, newCompilationUnit).toString();
    }

    /**
     * merge java bean
     *
     * @param existingCompilationUnit 旧的
     * @param newCompilationUnit      新的
     * @return merge 后的
     */
    private CompilationUnit mergerFile(CompilationUnit existingCompilationUnit, CompilationUnit newCompilationUnit) {
        CompilationUnit finalCompilationUnit = new CompilationUnit();

        // 修改包名为新类的包名
        if (newCompilationUnit.getPackageDeclaration().isPresent()) {
            finalCompilationUnit.setPackageDeclaration(newCompilationUnit.getPackageDeclaration().get());
        }

        // 合并import
        Set<ImportDeclaration> importSet = new LinkedHashSet<>();
        // 先放新的，再放旧的
        importSet.addAll(newCompilationUnit.getImports());
        importSet.addAll(existingCompilationUnit.getImports());

        NodeList<ImportDeclaration> imports = new NodeList<>();
        imports.addAll(importSet);
        finalCompilationUnit.setImports(imports);

        // 合并topLevelClass
        finalCompilationUnit.setTypes(this.mergeTypes(existingCompilationUnit.getTypes(), newCompilationUnit.getTypes()));

        return finalCompilationUnit;
    }

    /**
     * 合并Java类（一个Java文件可能有多个类）
     */
    private NodeList<TypeDeclaration<?>> mergeTypes(NodeList<TypeDeclaration<?>> oldTypes, NodeList<TypeDeclaration<?>> newTypes) {
        Map<String, TypeDeclaration<?>> finalTypes = newTypes.stream()
                .collect(Collectors.toMap(NodeWithSimpleName::getNameAsString, Function.identity(), (a, b) -> b, LinkedHashMap::new));

        for (TypeDeclaration<?> oldType : oldTypes) {
            // 对于旧CompilationUnit中的每一个TopLevelClass
            if (finalTypes.containsKey(oldType.getNameAsString())) {
                // 如果存在同名类则合并
                finalTypes.put(oldType.getNameAsString(), this.mergeType(oldType, finalTypes.get(oldType.getNameAsString())));
            } else if (!isGeneratedNode(oldType)) {
                // 如果不存在同名类且不是生成的类
                finalTypes.put(oldType.getNameAsString(), this.mergeType(oldType, finalTypes.get(oldType.getNameAsString())));
            }
        }

        return new NodeList<>(finalTypes.values());
    }

    /**
     * 合并两个同名类
     */
    private TypeDeclaration<?> mergeType(TypeDeclaration<?> oldType, TypeDeclaration<?> newType) {
        TypeDeclaration<?> finalTypeDeclaration;
        if (newType.isClassOrInterfaceDeclaration() && oldType.isClassOrInterfaceDeclaration()) {
            finalTypeDeclaration = new ClassOrInterfaceDeclaration();
            ClassOrInterfaceDeclaration oldClass = oldType.asClassOrInterfaceDeclaration();
            ClassOrInterfaceDeclaration newClass = newType.asClassOrInterfaceDeclaration();

            // 设置修饰符及类名
            // 修饰符
            finalTypeDeclaration.setModifiers(newClass.getModifiers());
            // 是否为接口
            finalTypeDeclaration.asClassOrInterfaceDeclaration().setInterface(newClass.isInterface());
            // 类名
            finalTypeDeclaration.setName(newClass.getName());
            // 继承的类
            finalTypeDeclaration.asClassOrInterfaceDeclaration().setExtendedTypes(newClass.getExtendedTypes());
            // 继承的接口
            finalTypeDeclaration.asClassOrInterfaceDeclaration().setImplementedTypes(newClass.getImplementedTypes());
            // 注解
            finalTypeDeclaration.asClassOrInterfaceDeclaration().setAnnotations(newClass.getAnnotations());
            // 注释
            if (newClass.getComment().isPresent()) {
                finalTypeDeclaration.asClassOrInterfaceDeclaration().setComment(newClass.getComment().get());
            }

            // 合并initializer(possibly static)
            // 保留所有旧类中的initializer（MBG并不会生成initializer，不考虑保留旧initializer会出现的问题）
            for (BodyDeclaration<?> bodyDeclaration : oldClass.getMembers()) {
                if (bodyDeclaration.isInitializerDeclaration()) {
                    finalTypeDeclaration.addMember(bodyDeclaration);
                }
            }

            // 合并构造函数
            for (ConstructorDeclaration constructorDeclaration : mergeConstructors(oldClass.getConstructors(), newClass.getConstructors())) {
                finalTypeDeclaration.addMember(constructorDeclaration);
            }

            // 合并Field
            for (FieldDeclaration fieldDeclaration : mergeFields(oldClass.getFields(), newClass.getFields())) {
                finalTypeDeclaration.addMember(fieldDeclaration);
            }

            // 合并Method
            for (MethodDeclaration methodDeclaration : mergeMethods(oldClass.getMethods(), newClass.getMethods())) {
                finalTypeDeclaration.addMember(methodDeclaration);
            }

            // 合并内部类（class/enum/interface）
            NodeList<TypeDeclaration<?>> oldTypes = new NodeList<>();
            NodeList<TypeDeclaration<?>> newTypes = new NodeList<>();
            for (BodyDeclaration<?> bodyDeclaration : oldClass.getMembers()) {
                if (bodyDeclaration.isClassOrInterfaceDeclaration() ||
                        bodyDeclaration.isEnumDeclaration() ||
                        bodyDeclaration.isAnnotationDeclaration()) {
                    oldTypes.add(bodyDeclaration.asTypeDeclaration());
                    // System.out.println("旧内部类：" + bodyDeclaration.asTypeDeclaration().getNameAsString());
                }
            }
            for (BodyDeclaration<?> bodyDeclaration : newClass.getMembers()) {
                if (bodyDeclaration.isClassOrInterfaceDeclaration() ||
                        bodyDeclaration.isEnumDeclaration() ||
                        bodyDeclaration.isAnnotationDeclaration()) {
                    newTypes.add(bodyDeclaration.asTypeDeclaration());
                    // System.out.println("新内部类：" + bodyDeclaration.asTypeDeclaration().getNameAsString());
                    // System.out.println("新内部类修饰符：" + bodyDeclaration.asTypeDeclaration().getModifiers());
                }
            }
            for (TypeDeclaration<?> typeDeclaration : mergeTypes(oldTypes, newTypes)) {
                finalTypeDeclaration.addMember(typeDeclaration);
            }

            return finalTypeDeclaration;
        } else if (newType.isEnumDeclaration() && oldType.isEnumDeclaration()) {
            return newType;
        } else if (newType.isAnnotationDeclaration() && oldType.isAnnotationDeclaration()) {
            return newType;
        } else {
            throw new RuntimeException(String.format("新类和旧类的类型不一样，无法判断该以何种方式合并，请删除旧文件或者将旧文件更改为正确的类型 (类名：%s)", newType.getNameAsString()));
        }
    }

    /**
     * 合并构造函数
     */
    private List<ConstructorDeclaration> mergeConstructors(List<ConstructorDeclaration> oldConstructors, List<ConstructorDeclaration> newConstructors) {

        Map<String, ConstructorDeclaration> constructorDeclarationMap = new LinkedHashMap<>();
        for (ConstructorDeclaration newConstructor : newConstructors) {
            if (!constructorDeclarationMap.containsKey(newConstructor.getDeclarationAsString(false, false, false))
                    && !isGeneratedNode(newConstructor)) {
                // 如果新生成的类中不包含该构造函数且该构造函数不是自动生成的
                constructorDeclarationMap.put(newConstructor.getDeclarationAsString(false, false, false), newConstructor);
            }
        }
        for (ConstructorDeclaration oldConstructor : oldConstructors) {
            constructorDeclarationMap.put(oldConstructor.getDeclarationAsString(false, false, false), oldConstructor);
        }
        return new ArrayList<>(constructorDeclarationMap.values());
    }

    /**
     * 合并字段
     */
    private List<FieldDeclaration> mergeFields(List<FieldDeclaration> oldFields, List<FieldDeclaration> newFields) {
        Map<String, FieldDeclaration> fieldDeclarationMap = new LinkedHashMap<>();
        for (FieldDeclaration newField : newFields) {
            // mbg生成的一个变量声明不会包含多个变量
            StringBuilder key = new StringBuilder();
            for (VariableDeclarator variableDeclarator : newField.getVariables()) {
                key.append(variableDeclarator.getNameAsString()).append(",");
            }
            fieldDeclarationMap.put(key.toString(), newField);
        }
        for (FieldDeclaration oldField : oldFields) {
            StringBuilder key = new StringBuilder();
            for (VariableDeclarator variableDeclarator : oldField.getVariables()) {
                key.append(variableDeclarator.getNameAsString()).append(",");
            }
            if (!fieldDeclarationMap.containsKey(key.toString()) && !isGeneratedNode(oldField)) {
                fieldDeclarationMap.put(key.toString(), oldField);
            }
        }
        return new ArrayList<>(fieldDeclarationMap.values());
    }

    /**
     * 合并方法
     *
     * @param oldMethods 旧方法
     * @param newMethods 新方法
     */
    private List<MethodDeclaration> mergeMethods(List<MethodDeclaration> oldMethods, List<MethodDeclaration> newMethods) {
        Map<String, MethodDeclaration> methodDeclarationMap = new LinkedHashMap<>();
        for (MethodDeclaration newMethod : newMethods) {
            methodDeclarationMap.put(newMethod.getDeclarationAsString(false, false, false), newMethod);
        }
        for (MethodDeclaration oldMethod : oldMethods) {
            if (!methodDeclarationMap.containsKey(oldMethod.getDeclarationAsString(false, false, false)) && !isGeneratedNode(oldMethod)) {
                methodDeclarationMap.put(oldMethod.getDeclarationAsString(false, false, false), oldMethod);
            }
        }
        return new ArrayList<>(methodDeclarationMap.values());
    }


    /**
     * 判断是否为自动生成的节点（即注释中包含指定tag）
     */
    private boolean isGeneratedNode(Node node) {
        final NodeList<AnnotationExpr> annotations = ((BodyDeclaration) node).getAnnotations();
        for (String tag : OLD_ELEMENT_TAGS) {
            for (AnnotationExpr annotation : annotations) {
                if (annotation.getName().asString().contains(tag)) {
                    return true;
                }
            }
            if (node.getComment().toString().contains(tag)) {
                return true;
            }
        }
        return false;
    }

    private static final List<String> OLD_ELEMENT_TAGS = List.of("Generated", MergeConstants.NEW_ELEMENT_TAG);
}
