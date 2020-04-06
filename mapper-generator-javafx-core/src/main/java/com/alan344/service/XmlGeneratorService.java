package com.alan344.service;

import com.alan344.bean.Column;
import com.alan344.bean.ColumnOverride;
import com.alan344.bean.GeneratorConfig;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.MyShellCallback;
import com.alan344happyframework.constants.SeparatorConstants;
import com.alan344happyframework.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.api.MyBatisGenerator;
import org.mybatis.generator.config.Configuration;
import org.mybatis.generator.config.xml.ConfigurationParser;
import org.mybatis.generator.exception.InvalidConfigurationException;
import org.mybatis.generator.exception.XMLParserException;
import org.springframework.stereotype.Service;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
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
    private DocumentBuilder documentBuilder;

    {
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        try {
            documentBuilder = documentBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            log.error("初始化错误", e);
        }
    }

    /**
     * 生成 xml 并用 mybatis-generator 生成对应的文件
     *
     * @param generatorConfig 配置文件
     */
    public void generatorXml(GeneratorConfig generatorConfig) {

        final org.w3c.dom.Document doc = documentBuilder.newDocument();
        final org.w3c.dom.Element root = doc.createElement("generatorConfiguration");
        doc.appendChild(root);

        final org.w3c.dom.Element context = this.addElement(doc, root, "context");
        context.setAttribute("id", "context1");
        context.setAttribute("targetRuntime", "MyBatis3");

        //指定编码
        final org.w3c.dom.Element javaFileEncodingProperty = this.addElement(doc, context, "property");
        javaFileEncodingProperty.setAttribute("name", "javaFileEncoding");
        javaFileEncodingProperty.setAttribute("value", "UTF-8");

        //使用java8 (默认是使用java8)
        final org.w3c.dom.Element targetJava8Property = this.addElement(doc, context, "property");
        targetJava8Property.setAttribute("name", "targetJava8");
        targetJava8Property.setAttribute("value", "true");

        final org.w3c.dom.Element serializablePlugin = this.addElement(doc, context, "plugin");
        serializablePlugin.setAttribute("type", "org.mybatis.generator.plugins.SerializablePlugin");
        if (generatorConfig.isUseMerge()) {
            final org.w3c.dom.Element unmergeableXmlMappersPlugin = this.addElement(doc, context, "plugin");
            unmergeableXmlMappersPlugin.setAttribute("type", "org.mybatis.generator.plugins.UnmergeableXmlMappersPlugin");
        }

        //是否成成注释
        if (generatorConfig.isUseComment()) {
            final org.w3c.dom.Element commentGenerator = this.addElement(doc, context, "commentGenerator");
            commentGenerator.setAttribute("type", "com.alan344.utils.MyCommentGenerator");
            final org.w3c.dom.Element suppressAllCommentsPro = this.addElement(doc, commentGenerator, "property");
            suppressAllCommentsPro.setAttribute("name", "suppressAllComments");
            suppressAllCommentsPro.setAttribute("value", "false");
            final org.w3c.dom.Element addRemarkCommentsPro = this.addElement(doc, commentGenerator, "property");
            addRemarkCommentsPro.setAttribute("name", "addRemarkComments");
            addRemarkCommentsPro.setAttribute("value", "true");
            final org.w3c.dom.Element suppressDatePro = this.addElement(doc, commentGenerator, "property");
            suppressDatePro.setAttribute("name", "suppressDate");
            suppressDatePro.setAttribute("value", "false");

            final org.w3c.dom.Element authorPro = this.addElement(doc, commentGenerator, "property");
            authorPro.setAttribute("name", "author");
            authorPro.setAttribute("value", generatorConfig.getAuthor());
        }

        //jdbc 连接
        final org.w3c.dom.Element jdbcConnection = this.addElement(doc, context, "jdbcConnection");
        jdbcConnection.setAttribute("driverClass", "com.mysql.cj.jdbc.Driver");
        jdbcConnection.setAttribute("connectionURL", "jdbc:mysql://" + BaseConstants.selectedDateSource.getHost() + "/" + BaseConstants.selectedDateSource.getDatabase() + "?nullCatalogMeansCurrent=true");
        jdbcConnection.setAttribute("userId", BaseConstants.selectedDateSource.getUser());
        jdbcConnection.setAttribute("password", BaseConstants.selectedDateSource.getPassword());

        //默认类型解析器
        if (generatorConfig.isUserJava8() && generatorConfig.isUseBigDecimal()) {
            final org.w3c.dom.Element javaTypeResolver = this.addElement(doc, context, "javaTypeResolver");

            final org.w3c.dom.Element useJSR310TypesProperty = this.addElement(doc, javaTypeResolver, "property");
            useJSR310TypesProperty.setAttribute("name", "useJSR310Types");
            useJSR310TypesProperty.setAttribute("value", "true");

            final org.w3c.dom.Element forceBigDecimals = this.addElement(doc, javaTypeResolver, "property");
            forceBigDecimals.setAttribute("name", "forceBigDecimals");
            forceBigDecimals.setAttribute("value", "true");
        } else if (generatorConfig.isUserJava8()) {
            final org.w3c.dom.Element javaTypeResolver = this.addElement(doc, context, "javaTypeResolver");

            final org.w3c.dom.Element useJSR310TypesProperty = this.addElement(doc, javaTypeResolver, "property");
            useJSR310TypesProperty.setAttribute("name", "useJSR310Types");
            useJSR310TypesProperty.setAttribute("value", "true");
        } else if (generatorConfig.isUseBigDecimal()) {
            final org.w3c.dom.Element javaTypeResolver = this.addElement(doc, context, "javaTypeResolver");

            final org.w3c.dom.Element forceBigDecimals = this.addElement(doc, javaTypeResolver, "forceBigDecimals");
            forceBigDecimals.setAttribute("name", "forceBigDecimals");
            forceBigDecimals.setAttribute("value", "true");
        }

        final org.w3c.dom.Element javaModelGenerator = this.addElement(doc, context, "javaModelGenerator");
        javaModelGenerator.setAttribute("targetPackage", generatorConfig.getBeanPackage());
        javaModelGenerator.setAttribute("targetProject", generatorConfig.getBeanLocation().replaceAll("\\\\", "/"));

        final org.w3c.dom.Element sqlMapGenerator = this.addElement(doc, context, "sqlMapGenerator");
        sqlMapGenerator.setAttribute("targetPackage", SeparatorConstants.DOT);
        sqlMapGenerator.setAttribute("targetProject", generatorConfig.getMapperXmlLocation().replaceAll("\\\\", "/"));

        final org.w3c.dom.Element javaClientGenerator = this.addElement(doc, context, "javaClientGenerator");
        javaClientGenerator.setAttribute("targetPackage", generatorConfig.getMapperPackage());
        javaClientGenerator.setAttribute("targetProject", generatorConfig.getMapperLocation().replaceAll("\\\\", "/"));
        javaClientGenerator.setAttribute("type", "XMLMAPPER");

        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {

            final org.w3c.dom.Element tableEl = this.addElement(doc, context, "table");
            tableEl.setAttribute("tableName", table.getTableName());

            List<Column> columns = table.getColumns();

            if (table.isReturnInsertId()) {
                final org.w3c.dom.Element generatedKey = this.addElement(doc, tableEl, "generatedKey");
                generatedKey.setAttribute("column", columns.get(0).getColumnName());
                generatedKey.setAttribute("sqlStatement", "JDBC");
            }

            this.checkBoxSelected("enableInsert", tableEl, table.isInsert());
            this.checkBoxSelected("enableCountByExample", tableEl, table.isCount());
            this.checkBoxSelected("enableUpdateByPrimaryKey", tableEl, table.isUpdate());
            this.checkBoxSelected("enableUpdateByExample", tableEl, table.isUpdateExample());
            this.checkBoxSelected("enableDeleteByPrimaryKey", tableEl, table.isDelete());
            this.checkBoxSelected("enableDeleteByExample", tableEl, table.isDeleteExample());
            this.checkBoxSelected("enableSelectByPrimaryKey", tableEl, table.isSelect());
            this.checkBoxSelected("enableSelectByExample", tableEl, table.isSelectExample());

            if (table.isJdkSerializable()) {
                final org.w3c.dom.Element property = this.addElement(doc, tableEl, "property");
                property.setAttribute("name", "jdkSerializable");
                property.setAttribute("value", "true");
            }

            // 是否使用原来的字段名
            if (generatorConfig.isUseActualColumnNames()) {
                tableEl.setAttribute("useActualColumnNames", "true");
            }

            columns.forEach(column -> {
                if (column.isIgnore()) {
                    final org.w3c.dom.Element ignoreColumn = this.addElement(doc, tableEl, "ignoreColumn");
                    ignoreColumn.setAttribute("column", column.getColumnName());
                }

                ColumnOverride columnOverride = column.getColumnOverride();
                if (columnOverride.isNotEmpty()) {
                    final org.w3c.dom.Element columnOverrideEl = this.addElement(doc, tableEl, "columnOverride");
                    columnOverrideEl.setAttribute("column", column.getColumnName());
                    if (StringUtils.isNotEmpty(columnOverride.getProperty())) {
                        columnOverrideEl.setAttribute("property", columnOverride.getProperty());
                    }
                    if (StringUtils.isNotEmpty(columnOverride.getJavaType())) {
                        columnOverrideEl.setAttribute("javaType", columnOverride.getJavaType());
                    }
                    if (StringUtils.isNotEmpty(columnOverride.getTypeHandler())) {
                        columnOverrideEl.setAttribute("typeHandler", columnOverride.getTypeHandler());
                    }
                    if (columnOverride.isGeneratedAlways()) {
                        columnOverrideEl.setAttribute("isGeneratedAlways", String.valueOf(columnOverride.isGeneratedAlways()));
                    }
                    if (columnOverride.isDelimitedColumnName()) {
                        columnOverrideEl.setAttribute("delimitedColumnName", String.valueOf(columnOverride.isDelimitedColumnName()));
                    }
                }
            });

        }

        // 判断文件夹是否存在，如果不存在则进行创建
        this.checkAndGeneratorDir(generatorConfig);
        // 执行创建
        this.generateMyBatis3(doc, generatorConfig);
    }

    private org.w3c.dom.Element addElement(org.w3c.dom.Document doc, org.w3c.dom.Element parent, String childName) {
        final org.w3c.dom.Element element = doc.createElement(childName);
        if (parent != null) {
            parent.appendChild(element);
        }

        return element;
    }

    private void checkBoxSelected(String name, org.w3c.dom.Element table, boolean flag) {
        if (flag) {
            table.setAttribute(name, Boolean.TRUE.toString());
        } else {
            table.setAttribute(name, Boolean.FALSE.toString());
        }
    }

    /**
     * 判断文件夹是否存在，如果不存在则进行创建
     */
    private void checkAndGeneratorDir(GeneratorConfig generatorConfig) {
        File beanFile = new File(generatorConfig.getBeanLocation());
        if (!beanFile.exists() && !beanFile.mkdirs()) {
            log.error("创建bean文件夹：{} 失败", generatorConfig.getBeanLocation());
        }

        File mapperFile = new File(generatorConfig.getMapperLocation());
        if (!mapperFile.exists() && !mapperFile.mkdirs()) {
            log.error("创建mapper文件夹：{} 失败", generatorConfig.getMapperLocation());
        }

        File xmlFile = new File(generatorConfig.getMapperXmlLocation());
        if (!xmlFile.exists() && !xmlFile.mkdirs()) {
            log.error("创建xml文件夹：{} 失败", generatorConfig.getMapperXmlLocation());
        }
    }


    /**
     * 调用 mybatis-generator
     *
     * @param document xml
     */
    private void generateMyBatis3(org.w3c.dom.Document document, GeneratorConfig generatorConfig) {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        try {
            Configuration config = cp.parseConfiguration(document);

            MyShellCallback shellCallback = new MyShellCallback(true, generatorConfig.isUseMerge());

            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null);
        } catch (InvalidConfigurationException | XMLParserException | InterruptedException | IOException | SQLException e) {
            log.error("生成mapper error", e);
        }

        if (!warnings.isEmpty()) {
            warnings.forEach(log::warn);
        }
    }
}