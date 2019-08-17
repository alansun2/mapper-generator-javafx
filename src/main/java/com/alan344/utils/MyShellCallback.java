package com.alan344.utils;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.PackageDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.printer.PrettyPrinterConfiguration;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.internal.DefaultShellCallback;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * @author AlanSun
 * @date 2019/8/17 19:43
 */
@Slf4j
public class MyShellCallback extends DefaultShellCallback {
    private boolean supportMerge;

    public MyShellCallback(boolean overwrite, boolean supportMerge) {
        super(overwrite);
        this.supportMerge = supportMerge;
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
        return mergerFile(newCompilationUnit, existingCompilationUnit);
    }

    private String mergerFile(CompilationUnit newCompilationUnit, CompilationUnit existingCompilationUnit) {

        Optional<PackageDeclaration> newPackageDeclaration = newCompilationUnit.getPackageDeclaration();
        newPackageDeclaration.ifPresent(existingCompilationUnit::setPackageDeclaration);

        //合并imports
        NodeList<ImportDeclaration> oldImports = existingCompilationUnit.getImports();
        NodeList<ImportDeclaration> newImports = newCompilationUnit.getImports();
        oldImports.addAll(newImports);
        Set<ImportDeclaration> importSet = new HashSet<>(oldImports);

        existingCompilationUnit.setImports(new NodeList<>(importSet));

        //处理类信息
        NodeList<TypeDeclaration<?>> types = newCompilationUnit.getTypes();
        NodeList<TypeDeclaration<?>> oldTypes = existingCompilationUnit.getTypes();

        for (int i = 0; i < types.size(); i++) {
            //合并fields
            TypeDeclaration<?> existingTypeDeclaration = oldTypes.get(i);
            List<FieldDeclaration> oldFields = existingTypeDeclaration.getFields();
            List<FieldDeclaration> fields = types.get(i).getFields();
            Set<FieldDeclaration> fieldDeclarations = new HashSet<>();
            fieldDeclarations.addAll(oldFields);
            fieldDeclarations.addAll(fields);

            oldFields.forEach(existingTypeDeclaration::remove);

            fieldDeclarations.forEach(existingTypeDeclaration::addMember);

            //合并methods
            List<MethodDeclaration> existingMethods = existingTypeDeclaration.getMethods();
            List<MethodDeclaration> methods = types.get(i).getMethods();
            Set<MethodDeclaration> methodDeclarationHashSet = new HashSet<>();
            methodDeclarationHashSet.addAll(existingMethods);
            methodDeclarationHashSet.addAll(methods);

            methodDeclarationHashSet.forEach(existingTypeDeclaration::remove);

            methodDeclarationHashSet.forEach(existingTypeDeclaration::addMember);
        }

        return existingCompilationUnit.toString();
    }
}
