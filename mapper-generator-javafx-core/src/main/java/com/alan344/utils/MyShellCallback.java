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
import org.mybatis.generator.api.GeneratedXmlFile;
import org.mybatis.generator.exception.ShellException;
import org.mybatis.generator.internal.DefaultShellCallback;
import org.mybatis.generator.internal.DomWriter;
import org.w3c.dom.*;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.mybatis.generator.internal.util.messages.Messages.getString;

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

    @Override
    public String mergeXmlFile(GeneratedXmlFile gxf, File targetFile) throws ShellException {

        try {
            return getMergedSource(new InputSource(new StringReader(gxf.getFormattedContent())),
                    new InputSource(new InputStreamReader(new FileInputStream(targetFile), StandardCharsets.UTF_8)),
                    targetFile.getName());
        } catch (IOException | SAXException | ParserConfigurationException e) {
            throw new ShellException(getString("Warning.13",
                    targetFile.getName()), e);
        }
    }

    private static String getMergedSource(InputSource newFile,
                                          InputSource existingFile, String existingFileName) throws IOException, SAXException,
            ParserConfigurationException, ShellException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setExpandEntityReferences(false);
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        DocumentBuilder builder = factory.newDocumentBuilder();
        builder.setEntityResolver(new NullEntityResolver());

        Document existingDocument = builder.parse(existingFile);
        existingDocument.setStrictErrorChecking(false);
        Document newDocument = builder.parse(newFile);

        DocumentType newDocType = newDocument.getDoctype();
        DocumentType existingDocType = existingDocument.getDoctype();

        if (!newDocType.getName().equals(existingDocType.getName())) {
            throw new ShellException(getString("Warning.12",
                    existingFileName));
        }

        Element existingRootElement = existingDocument.getDocumentElement();
        Element newRootElement = newDocument.getDocumentElement();

        // reconcile the root element attributes -
        // take all attributes from the new element and add to the existing
        // element

        // remove all attributes from the existing root element
        NamedNodeMap attributes = existingRootElement.getAttributes();
        int attributeCount = attributes.getLength();
        for (int i = attributeCount - 1; i >= 0; i--) {
            Node node = attributes.item(i);
            existingRootElement.removeAttribute(node.getNodeName());
        }

        // add attributes from the new root node to the old root node
        attributes = newRootElement.getAttributes();
        attributeCount = attributes.getLength();
        for (int i = 0; i < attributeCount; i++) {
            Node node = attributes.item(i);
            existingRootElement.setAttribute(node.getNodeName(), node.getNodeValue());
        }

        // remove the old generated elements and any
        // white space before the old nodes
        org.w3c.dom.NodeList existChildren = existingRootElement.getChildNodes();
        int existLength = existChildren.getLength();
        Map<String, Node> existIdChildMap = new HashMap<>();

        for (int i = 0; i < existLength; i++) {
            Node node = existChildren.item(i);
            if (!isWhiteSpace(node)) {
                existIdChildMap.put(node.getAttributes().getNamedItem("id").toString(), node);
            }
        }

        org.w3c.dom.NodeList newChildren = newRootElement.getChildNodes();
        int newLength = newChildren.getLength();
        Map<String, Node> newIdChildMap = new HashMap<>();
        for (int i = 0; i < newLength; i++) {
            Node node = newChildren.item(i);
            if (!isWhiteSpace(node)) {
                newIdChildMap.put(node.getAttributes().getNamedItem("id").toString(), node);
            }
        }

        existIdChildMap.forEach((k, v) -> {
            if (newIdChildMap.containsKey(k)) {
                existingRootElement.replaceChild(newIdChildMap.get(k), v);
                newIdChildMap.remove(k);
            }
        });

        Text whiteNode = existingDocument.createTextNode("\n  ");
        for (Map.Entry<String, Node> entry : newIdChildMap.entrySet()) {
            Node node = entry.getValue();
            Node whiteNodeClone = whiteNode.cloneNode(true);
            existingRootElement.appendChild(whiteNodeClone);
            existingRootElement.appendChild(node);
        }

        if (!newIdChildMap.isEmpty()) {
            Text textNode = existingDocument.createTextNode("\n");
            existingRootElement.appendChild(textNode);
        }

        // pretty print the result
        return prettyPrint(existingDocument);
    }

    private static String prettyPrint(Document document) throws ShellException {
        DomWriter dw = new DomWriter();
        return dw.toString(document);
    }

    private static boolean isWhiteSpace(Node node) {
        boolean rc = false;

        if (node != null && node.getNodeType() == Node.TEXT_NODE) {
            Text tn = (Text) node;
            if (tn.getData().trim().length() == 0) {
                rc = true;
            }
        }

        return rc;
    }

    private static class NullEntityResolver implements EntityResolver {
        /**
         * returns an empty reader. This is done so that the parser doesn't
         * attempt to read a DTD. We don't need that support for the merge and
         * it can cause problems on systems that aren't Internet connected.
         */
        @Override
        public InputSource resolveEntity(String publicId, String systemId)
                throws SAXException, IOException {

            StringReader sr = new StringReader("");

            return new InputSource(sr);
        }
    }
}
