package com.alan344.service;

import com.alan344.bean.Column;
import com.alan344.bean.ColumnOverride;
import com.alan344.bean.GeneratorConfig;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.HRXMLWriter;
import com.alan344.utils.MyShellCallback;
import com.alan344happyframework.constants.SeparatorConstants;
import com.alan344happyframework.util.StringUtils;
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
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
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
     * @throws Exception e
     */
    @Async
    public void generatorXml(GeneratorConfig generatorConfig) throws Exception {
        Document document = DocumentHelper.createDocument();
        document.addDocType("generatorConfiguration", "-//mybatis.org//DTD MyBatis Generator Configuration 1.0//EN", "mybatis-generator-config_1_0.dtd");

        Element root = document.addElement("generatorConfiguration");

        Element context = root.addElement("context");
        context.addAttribute("id", "context1");
        context.addAttribute("targetRuntime", "MyBatis3");

        //指定编码
        Element javaFileEncodingProperty = context.addElement("property");
        javaFileEncodingProperty.addAttribute("name", "javaFileEncoding");
        javaFileEncodingProperty.addAttribute("value", "UTF-8");

        //使用java8 (默认是使用java8)
        Element targetJava8Property = context.addElement("property");
        targetJava8Property.addAttribute("name", "targetJava8");
        targetJava8Property.addAttribute("value", "true");

        if (!generatorConfig.isUserMerge()) {
            Element UnmergeableXmlMappersPlugin = context.addElement("plugin");
            UnmergeableXmlMappersPlugin.addAttribute("type", "org.mybatis.generator.plugins.UnmergeableXmlMappersPlugin");
        }

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
        jdbcConnection.addAttribute("connectionURL", "jdbc:mysql://" + BaseConstants.selectedDateSource.getHost() + "/" + BaseConstants.selectedDateSource.getDatabase() + "?nullCatalogMeansCurrent=true");
        jdbcConnection.addAttribute("userId", BaseConstants.selectedDateSource.getUser());
        jdbcConnection.addAttribute("password", BaseConstants.selectedDateSource.getPassword());

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

        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {

            Element tableEl = context.addElement("table");
            tableEl.addAttribute("tableName", table.getTableName());

            List<Column> columns = table.getColumns();

            if (table.isReturnInsertId()) {
                Element generatedKey = tableEl.addElement("generatedKey");
                generatedKey.addAttribute("column", columns.get(0).getColumnName());
                generatedKey.addAttribute("sqlStatement", "JDBC");
            }

            this.checkBoxSelected("enableInsert", tableEl, table.isInsert());
            this.checkBoxSelected("enableCountByExample", tableEl, table.isCount());
            this.checkBoxSelected("enableUpdateByPrimaryKey", tableEl, table.isUpdate());
            this.checkBoxSelected("enableUpdateByExample", tableEl, table.isUpdateExample());
            this.checkBoxSelected("enableDeleteByPrimaryKey", tableEl, table.isDelete());
            this.checkBoxSelected("enableDeleteByExample", tableEl, table.isDeleteExample());
            this.checkBoxSelected("enableSelectByPrimaryKey", tableEl, table.isSelect());
            this.checkBoxSelected("enableSelectByExample", tableEl, table.isSelectExample());

            if (generatorConfig.isUseActualColumnNames()) {
                tableEl.addAttribute("useActualColumnNames", "true");
            }

            columns.forEach(column -> {
                if (column.isIgnore()) {
                    Element ignoreColumn = tableEl.addElement("ignoreColumn");
                    ignoreColumn.addAttribute("column", column.getColumnName());
                }

                ColumnOverride columnOverride = column.getColumnOverride();
                if (columnOverride.isNotEmpty()) {
                    Element columnOverrideEl = tableEl.addElement("columnOverride");
                    columnOverrideEl.addAttribute("column", column.getColumnName());
                    if (StringUtils.isNotEmpty(columnOverride.getProperty())) {
                        columnOverrideEl.addAttribute("property", columnOverride.getProperty());
                    }
                    if (StringUtils.isNotEmpty(columnOverride.getJavaType())) {
                        columnOverrideEl.addAttribute("javaType", columnOverride.getJavaType());
                    }
                    if (StringUtils.isNotEmpty(columnOverride.getTypeHandler())) {
                        columnOverrideEl.addAttribute("typeHandler", columnOverride.getTypeHandler());
                    }
                    if (columnOverride.isGeneratedAlways()) {
                        columnOverrideEl.addAttribute("isGeneratedAlways", String.valueOf(columnOverride.isGeneratedAlways()));
                    }
                    if (columnOverride.isDelimitedColumnName()) {
                        columnOverrideEl.addAttribute("delimitedColumnName", String.valueOf(columnOverride.isDelimitedColumnName()));
                    }
                }
            });

        }

        String generatorConfigName = System.getProperty("user.dir") + "/generatorConfig.xml";
        Writer fileWriter = new FileWriter(generatorConfigName);
        XMLWriter xmlWriter = new HRXMLWriter(fileWriter, OutputFormat.createPrettyPrint());
        xmlWriter.write(document);
        xmlWriter.close();

        this.generateMyBatis3WithInvalidConfig(generatorConfigName, generatorConfig);
    }

    private void checkBoxSelected(String name, Element table, boolean flag) {
        if (flag) {
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
    private void generateMyBatis3WithInvalidConfig(String fileName, GeneratorConfig generatorConfig) throws Exception {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        Configuration config = cp.parseConfiguration(new FileInputStream(fileName));

        MyShellCallback shellCallback = new MyShellCallback(true, generatorConfig.isUserMerge());

        try {
            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null);
        } catch (InvalidConfigurationException e) {
            log.error("生成mapper error", e);
        }
    }
}