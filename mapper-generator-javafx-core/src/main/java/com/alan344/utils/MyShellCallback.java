package com.alan344.utils;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.PackageDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
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
 * merge 现在不用，有点问题
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

    /**
     * merge java bean
     *
     * @param newCompilationUnit      新的
     * @param existingCompilationUnit 旧的
     * @return merge 后的
     */
    private String mergerFile(CompilationUnit newCompilationUnit, CompilationUnit existingCompilationUnit) {

        Optional<PackageDeclaration> newPackageDeclaration = newCompilationUnit.getPackageDeclaration();
        newPackageDeclaration.ifPresent(existingCompilationUnit::setPackageDeclaration);

        //合并imports
        NodeList<ImportDeclaration> oldImports = existingCompilationUnit.getImports();
        NodeList<ImportDeclaration> newImports = newCompilationUnit.getImports();
        oldImports.addAll(newImports);
        Set<ImportDeclaration> importSet = new HashSet<>(oldImports);

        existingCompilationUnit.setImports(new NodeList<>(importSet));

        //处理类 comment
        TypeDeclaration<?> newType = newCompilationUnit.getTypes().get(0);
        TypeDeclaration<?> existType = existingCompilationUnit.getTypes().get(0);
        newType.getComment().ifPresent(existType::setComment);

        List<FieldDeclaration> existFields = existType.getFields();
        List<FieldDeclaration> newFields = newType.getFields();

        //合并fields
        int size = newFields.size();
        for (int i = 0; i < size; i++) {
            FieldDeclaration existField = newFields.get(0);
            VariableDeclarator existVar = existField.getVariables().get(0);
            for (FieldDeclaration newField : existFields) {
                VariableDeclarator newVar = newField.getVariables().get(0);
                // 名称相同
                if (newVar.getName().equals(existVar.getName())) {
                    // 名称相同 且 类型相同
                    if (newVar.getTypeAsString().equals(existVar.getTypeAsString())) {
                        newType.getComment().ifPresent(existType::setComment);
                    } else {

                    }
                }
            }

            //合并methods
        }

        return existingCompilationUnit.toString();
    }
}
