package com.alan344.service.generator;

import com.alan344.bean.Column;
import com.alan344.bean.ColumnOverride;
import com.alan344.bean.GeneratorConfig;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344happyframework.constants.SeparatorConstants;
import com.alan344happyframework.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.Collection;
import java.util.List;

/**
 * @author AlanSun
 * @date 2020/6/5 12:37
 */
@Slf4j
@Service
public class MapperBaseGenerator extends MapperGeneratorStrategyBase {
    /**
     * 生成 xml 并用 mybatis-generator 生成对应的文件
     *
     * @param generatorConfig 配置文件
     */
    @Override
    public void generator(GeneratorConfig generatorConfig) {
        final Document doc = documentBuilder.newDocument();
        final Element root = doc.createElement("generatorConfiguration");
        doc.appendChild(root);

        final Element context = this.addElement(doc, root, "context");
        context.setAttribute("id", "context1");
        context.setAttribute("targetRuntime", generatorConfig.getTargetName());

        //指定编码
        final Element javaFileEncodingProperty = this.addElement(doc, context, "property");
        javaFileEncodingProperty.setAttribute("name", "javaFileEncoding");
        javaFileEncodingProperty.setAttribute("value", "UTF-8");

        //使用java8 (默认是使用java8)
        final Element targetJava8Property = this.addElement(doc, context, "property");
        targetJava8Property.setAttribute("name", "targetJava8");
        targetJava8Property.setAttribute("value", "true");

        final Element serializablePlugin = this.addElement(doc, context, "plugin");
        serializablePlugin.setAttribute("type", "org.mybatis.generator.plugins.SerializablePlugin");

        if (generatorConfig.isUseMerge()) {
            final Element unmergeableXmlMappersPlugin = this.addElement(doc, context, "plugin");
            unmergeableXmlMappersPlugin.setAttribute("type", "org.mybatis.generator.plugins.UnmergeableXmlMappersPlugin");
        }
        //    <plugin type="tk.mybatis.mapper.generator.MapperPlugin">
        //            <property name="mappers" value="tk.mybatis.mapper.common.Mapper"/>
        //            <property name="caseSensitive" value="true"/>
        //            <property name="forceAnnotation" value="true"/>
        //            <property name="beginningDelimiter" value="`"/>
        //            <property name="endingDelimiter" value="`"/>
        //        </plugin>
        final Element tkMybatisPlugin = this.addElement(doc, context, "plugin");
        tkMybatisPlugin.setAttribute("type", "tk.mybatis.mapper.generator.MapperPlugin");
        final Element mappersProperty = this.addElement(doc, tkMybatisPlugin, "property");
        mappersProperty.setAttribute("name", "mappers");
        mappersProperty.setAttribute("value", "tk.mybatis.mapper.common.Mapper");
        final Element useMapperCommentGeneratorProperty = this.addElement(doc, tkMybatisPlugin, "property");
        useMapperCommentGeneratorProperty.setAttribute("name", "useMapperCommentGenerator");
        useMapperCommentGeneratorProperty.setAttribute("value", "true");


        //是否成成注释
        if (generatorConfig.isUseComment()) {
            final Element commentGenerator = this.addElement(doc, context, "commentGenerator");
            commentGenerator.setAttribute("type", "com.alan344.utils.MyCommentGenerator");
            final Element suppressAllCommentsPro = this.addElement(doc, commentGenerator, "property");
            suppressAllCommentsPro.setAttribute("name", "suppressAllComments");
            suppressAllCommentsPro.setAttribute("value", "false");
            final Element addRemarkCommentsPro = this.addElement(doc, commentGenerator, "property");
            addRemarkCommentsPro.setAttribute("name", "addRemarkComments");
            addRemarkCommentsPro.setAttribute("value", "true");
            final Element suppressDatePro = this.addElement(doc, commentGenerator, "property");
            suppressDatePro.setAttribute("name", "suppressDate");
            suppressDatePro.setAttribute("value", "false");

            final Element authorPro = this.addElement(doc, commentGenerator, "property");
            authorPro.setAttribute("name", "author");
            authorPro.setAttribute("value", generatorConfig.getAuthor());
        }

        //jdbc 连接
        final Element jdbcConnection = this.addElement(doc, context, "jdbcConnection");
        jdbcConnection.setAttribute("driverClass", "com.mysql.cj.jdbc.Driver");
        jdbcConnection.setAttribute("connectionURL", "jdbc:mysql://" + BaseConstants.selectedDateSource.getHost() + "/" + BaseConstants.selectedDateSource.getDatabase() + "?nullCatalogMeansCurrent=true");
        jdbcConnection.setAttribute("userId", BaseConstants.selectedDateSource.getUser());
        jdbcConnection.setAttribute("password", BaseConstants.selectedDateSource.getPassword());

        //默认类型解析器
        if (generatorConfig.isUserJava8() && generatorConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = this.addElement(doc, context, "javaTypeResolver");

            final Element useJSR310TypesProperty = this.addElement(doc, javaTypeResolver, "property");
            useJSR310TypesProperty.setAttribute("name", "useJSR310Types");
            useJSR310TypesProperty.setAttribute("value", "true");

            final Element forceBigDecimals = this.addElement(doc, javaTypeResolver, "property");
            forceBigDecimals.setAttribute("name", "forceBigDecimals");
            forceBigDecimals.setAttribute("value", "true");
        } else if (generatorConfig.isUserJava8()) {
            final Element javaTypeResolver = this.addElement(doc, context, "javaTypeResolver");

            final Element useJSR310TypesProperty = this.addElement(doc, javaTypeResolver, "property");
            useJSR310TypesProperty.setAttribute("name", "useJSR310Types");
            useJSR310TypesProperty.setAttribute("value", "true");
        } else if (generatorConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = this.addElement(doc, context, "javaTypeResolver");

            final Element forceBigDecimals = this.addElement(doc, javaTypeResolver, "forceBigDecimals");
            forceBigDecimals.setAttribute("name", "forceBigDecimals");
            forceBigDecimals.setAttribute("value", "true");
        }

        // model 位置配置
        final Element javaModelGenerator = this.addElement(doc, context, "javaModelGenerator");
        javaModelGenerator.setAttribute("targetPackage", generatorConfig.getBeanPackage());
        javaModelGenerator.setAttribute("targetProject", generatorConfig.getBeanLocation().replaceAll("\\\\", "/"));

        // xml 位置配置
        if (StringUtils.isNotEmpty(generatorConfig.getMapperXmlLocation())) {
            final Element sqlMapGenerator = this.addElement(doc, context, "sqlMapGenerator");
            sqlMapGenerator.setAttribute("targetPackage", SeparatorConstants.DOT);
            sqlMapGenerator.setAttribute("targetProject", generatorConfig.getMapperXmlLocation().replaceAll("\\\\", "/"));
        }

        // mapper （dao） 位置配置
        final Element javaClientGenerator = this.addElement(doc, context, "javaClientGenerator");
        javaClientGenerator.setAttribute("targetPackage", generatorConfig.getMapperPackage());
        javaClientGenerator.setAttribute("targetProject", generatorConfig.getMapperLocation().replaceAll("\\\\", "/"));
        javaClientGenerator.setAttribute("type", "XMLMAPPER");

        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {

            final Element tableEl = this.addElement(doc, context, "table");
            tableEl.setAttribute("tableName", table.getTableName());

            List<Column> columns = table.getColumns();

            if (table.isReturnInsertId()) {
                final Element generatedKey = this.addElement(doc, tableEl, "generatedKey");
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
                final Element property = this.addElement(doc, tableEl, "property");
                property.setAttribute("name", "jdkSerializable");
                property.setAttribute("value", "true");
            }

            // 是否使用原来的字段名
            if (generatorConfig.isUseActualColumnNames()) {
                tableEl.setAttribute("useActualColumnNames", "true");
            }

            columns.forEach(column -> {
                if (column.isIgnore()) {
                    final Element ignoreColumn = this.addElement(doc, tableEl, "ignoreColumn");
                    ignoreColumn.setAttribute("column", column.getColumnName());
                }

                ColumnOverride columnOverride = column.getColumnOverride();
                if (columnOverride.isNotEmpty()) {
                    final Element columnOverrideEl = this.addElement(doc, tableEl, "columnOverride");
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
}
