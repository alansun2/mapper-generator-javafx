package com.alan344.service;

import com.alan344.bean.GeneratorConfig;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.HRXMLWriter;
import com.alan344happyframework.constants.SeparatorConstants;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.CheckBox;
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
import org.springframework.scheduling.annotation.Async;
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

    /**
     * 导出
     *
     * @param vBoxes listView的内容
     * @throws Exception e
     */
    @Async
    public void generatorXml(ObservableList<VBox> vBoxes, GeneratorConfig generatorConfig) throws Exception {
        Document document = DocumentHelper.createDocument();
        document.addDocType("generatorConfiguration", "-//mybatis.org//DTD MyBatis Generator Configuration 1.0//EN", "mybatis-generator-config_1_0.dtd");

        Element root = document.addElement("generatorConfiguration");

        Element context = root.addElement("context");
        context.addAttribute("id", "context1");
        context.addAttribute("targetRuntime", "MyBatis3");

        //是否成成注释
        if (generatorConfig.isUseComment()) {
            Element commentGenerator = context.addElement("commentGenerator");
            commentGenerator.addAttribute("type", "com.alan344.utils.MyCommentGenerator");
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
            authorPro.addAttribute("value", generatorConfig.getAuthor());
        }

        //jdbc 连接
        Element jdbcConnection = context.addElement("jdbcConnection");
        jdbcConnection.addAttribute("driverClass", "com.mysql.cj.jdbc.Driver");
        jdbcConnection.addAttribute("connectionURL", "jdbc:mysql://" + BaseConstants.currentDateSource.getHost() + "/" + BaseConstants.currentDateSource.getDatabase() + "?nullCatalogMeansCurrent=true");
        jdbcConnection.addAttribute("userId", BaseConstants.currentDateSource.getUser());
        jdbcConnection.addAttribute("password", BaseConstants.currentDateSource.getPassword());

        //默认类型解析器
        if (generatorConfig.isUserJava8() && generatorConfig.isUseBigDecimal()) {
            Element javaTypeResolver = context.addElement("javaTypeResolver");

            Element useJSR310TypesProperty = javaTypeResolver.addElement("property");
            useJSR310TypesProperty.addAttribute("name", "useJSR310Types");
            useJSR310TypesProperty.addAttribute("value", "true");

            Element forceBigDecimals = javaTypeResolver.addElement("forceBigDecimals");
            forceBigDecimals.addAttribute("name", "forceBigDecimals");
            forceBigDecimals.addAttribute("value", "true");
        } else if (generatorConfig.isUserJava8()) {
            Element javaTypeResolver = context.addElement("javaTypeResolver");

            Element useJSR310TypesProperty = javaTypeResolver.addElement("property");
            useJSR310TypesProperty.addAttribute("name", "useJSR310Types");
            useJSR310TypesProperty.addAttribute("value", "true");
        } else if (generatorConfig.isUseBigDecimal()) {
            Element javaTypeResolver = context.addElement("javaTypeResolver");

            Element forceBigDecimals = javaTypeResolver.addElement("forceBigDecimals");
            forceBigDecimals.addAttribute("name", "forceBigDecimals");
            forceBigDecimals.addAttribute("value", "true");
        }

        Element javaModelGenerator = context.addElement("javaModelGenerator");
        javaModelGenerator.addAttribute("targetPackage", generatorConfig.getBeanPackage());
        javaModelGenerator.addAttribute("targetProject", generatorConfig.getBeanLocation().replaceAll("\\\\", "/"));

        Element sqlMapGenerator = context.addElement("sqlMapGenerator");
        sqlMapGenerator.addAttribute("targetPackage", SeparatorConstants.DOT);
        sqlMapGenerator.addAttribute("targetProject", generatorConfig.getMapperXmlLocation().replaceAll("\\\\", "/"));

        Element javaClientGenerator = context.addElement("javaClientGenerator");
        javaClientGenerator.addAttribute("targetPackage", generatorConfig.getMapperPackage());
        javaClientGenerator.addAttribute("targetProject", generatorConfig.getMapperLocation().replaceAll("\\\\", "/"));
        javaClientGenerator.addAttribute("type", "XMLMAPPER");

        for (VBox vBox : vBoxes) {
            ObservableList<Node> children = vBox.getChildren();

            Label tableNameLabel = (Label) ((HBox) children.get(0)).getChildren().get(0);
            Element tableEl = context.addElement("table");
            String tableName = tableNameLabel.getText();
            tableEl.addAttribute("tableName", tableName);

            HBox secondHBox = (HBox) children.get(1);
            CheckBox insertReturnCheckBox = (CheckBox) secondHBox.getChildren().get(0);
            if (insertReturnCheckBox.isSelected()) {
                Element generatedKey = tableEl.addElement("generatedKey");
                Table table = BaseConstants.tableNameTableMap.get(tableName);
                generatedKey.addAttribute("column", table.getColumns().get(0).getColumnName());
                generatedKey.addAttribute("sqlStatement", "JDBC");
            }

            this.checkBoxSelected("enableInsert", 1, tableEl, secondHBox);
            this.checkBoxSelected("enableCountByExample", 2, tableEl, secondHBox);
            this.checkBoxSelected("enableUpdateByPrimaryKey", 3, tableEl, secondHBox);
            this.checkBoxSelected("enableUpdateByExample", 3, tableEl, secondHBox);
            this.checkBoxSelected("enableDeleteByPrimaryKey", 4, tableEl, secondHBox);
            this.checkBoxSelected("enableDeleteByExample", 4, tableEl, secondHBox);
            this.checkBoxSelected("enableSelectByExample", 5, tableEl, secondHBox);
            this.checkBoxSelected("enableSelectByPrimaryKey", 5, tableEl, secondHBox);
        }

        String generatorConfigName = System.getProperty("user.dir") + "/generatorConfig.xml";
        Writer fileWriter = new FileWriter(generatorConfigName);
        XMLWriter xmlWriter = new HRXMLWriter(fileWriter, OutputFormat.createPrettyPrint());
        xmlWriter.write(document);
        xmlWriter.close();

        this.generateMyBatis3WithInvalidConfig(generatorConfigName);
    }

    private void checkBoxSelected(String name, int index, Element table, HBox hBox) {
        CheckBox checkBox = (CheckBox) hBox.getChildren().get(index);
        if (checkBox.isSelected()) {
            table.addAttribute(name, Boolean.TRUE.toString());
        } else {
            table.addAttribute(name, Boolean.FALSE.toString());
        }
    }

    /**
     * 调用mybatis-generator
     *
     * @param fileName 配置文件地址
     * @throws Exception e
     */
    private void generateMyBatis3WithInvalidConfig(String fileName) throws Exception {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        Configuration config = cp.parseConfiguration(new FileInputStream(fileName));

        DefaultShellCallback shellCallback = new DefaultShellCallback(true);

        try {
            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null);
        } catch (InvalidConfigurationException e) {
            log.error("生成mapper error", e);
        }
    }
}