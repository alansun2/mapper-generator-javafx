package com.alan344.service;

import com.alan344.utils.HRXMLWriter;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import lombok.extern.slf4j.Slf4j;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.XMLWriter;
import org.mybatis.generator.api.MyBatisGenerator;
import org.mybatis.generator.config.Configuration;
import org.mybatis.generator.config.xml.ConfigurationParser;
import org.mybatis.generator.exception.InvalidConfigurationException;
import org.mybatis.generator.internal.DefaultShellCallback;
import org.springframework.stereotype.Service;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/9 16:19
 */
@Slf4j
@Service
public class XmlGeneratorService {

    public void generatorXml(ObservableList<VBox> vBoxes) throws Exception {
        Document document = DocumentHelper.createDocument();
        document.addDocType("generatorConfiguration", "-//mybatis.org//DTD MyBatis Generator Configuration 1.0//EN", "mybatis-generator-config_1_0.dtd");

        Element root = document.addElement("generatorConfiguration");

        Element context = root.addElement("context");
        context.addAttribute("id", "context1");
        context.addAttribute("targetRuntime", "MyBatis3");

        Element commentGenerator = context.addElement("commentGenerator");
        Element suppressAllCommentsPro = commentGenerator.addElement("property");
        suppressAllCommentsPro.addAttribute("name", "suppressAllComments");
        suppressAllCommentsPro.addAttribute("value", "false");
        Element addRemarkCommentsPro = commentGenerator.addElement("property");
        addRemarkCommentsPro.addAttribute("name", "addRemarkComments");
        addRemarkCommentsPro.addAttribute("value", "true");
        Element suppressDatePro = commentGenerator.addElement("property");
        suppressDatePro.addAttribute("name", "suppressDate");
        suppressDatePro.addAttribute("value", "false");
        Element authorPro = commentGenerator.addElement("property");
        authorPro.addAttribute("name", "author");
        authorPro.addAttribute("value", "AlanSun");

        Element jdbcConnection = context.addElement("jdbcConnection");
        jdbcConnection.addAttribute("driverClass", "com.mysql.jdbc.Driver");
        jdbcConnection.addAttribute("connectionURL", "jdbc:mysql://test.ehoo100.com/ehu_presell");
        jdbcConnection.addAttribute("userId", "root");
        jdbcConnection.addAttribute("password", "eHu2016");

        Element javaModelGenerator = context.addElement("javaModelGenerator");
        javaModelGenerator.addAttribute("targetPackage", "com.ehu.bean.model");
        javaModelGenerator.addAttribute("targetProject", "./src/main/java");

        Element sqlMapGenerator = context.addElement("sqlMapGenerator");
        sqlMapGenerator.addAttribute("targetPackage", ".");
        sqlMapGenerator.addAttribute("targetProject", "./src/main/resources/mapper");

        Element javaClientGenerator = context.addElement("javaClientGenerator");
        javaClientGenerator.addAttribute("targetPackage", "com.ehu.mapper");
        javaClientGenerator.addAttribute("targetProject", "./src/main/java");
        javaClientGenerator.addAttribute("type", "XMLMAPPER");

        for (VBox tableTreeItem : vBoxes) {
            ObservableList<Node> children = tableTreeItem.getChildren();
            Label tableNameLabel = (Label) ((HBox) children.get(0)).getChildren().get(0);
            Element table = context.addElement("table");
            table.addAttribute("tableName", tableNameLabel.getText());
        }

        OutputFormat outputFormat = OutputFormat.createPrettyPrint();
//        outputFormat.setIndent(true); //设置是否缩进
//        outputFormat.setIndent("\t"); //以四个空格方式实现缩进
//        outputFormat.setNewlines(true); //设置是否换行
//        outputFormat.setLineSeparator("\n\n");

        String generatorConfigName = System.getProperty("user.dir") + "/generatorConfig.xml";
        Writer fileWriter = new FileWriter(generatorConfigName);
        XMLWriter xmlWriter = new HRXMLWriter(fileWriter, outputFormat);
        xmlWriter.write(document);
        xmlWriter.close();


        testGenerateMyBatis3WithInvalidConfig(generatorConfigName);
    }

    private void testGenerateMyBatis3WithInvalidConfig(String fileName) throws Exception {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        Configuration config = cp.parseConfiguration(new FileInputStream(fileName));

        DefaultShellCallback shellCallback = new DefaultShellCallback(true);

        try {
            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null, false);
        } catch (InvalidConfigurationException e) {
            log.error("生成xml", e);
        }
    }
}