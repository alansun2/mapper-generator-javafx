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
        final GeneratorConfig.TkMybatisExportConfig tkMybatisExportConfig = generatorConfig.getTkMybatisExportConfig();

        doc = documentBuilder.newDocument();
        final Element root = doc.createElement("generatorConfiguration");
        doc.appendChild(root);

        final Element context = this.addElement(root, "context");
        context.setAttribute("id", "context1");
        context.setAttribute("targetRuntime", "MYBATIS3_TK");

        //指定编码
        final Element javaFileEncodingProperty = this.addElement(context, PROPERTY);
        javaFileEncodingProperty.setAttribute(NAME, "javaFileEncoding");
        javaFileEncodingProperty.setAttribute(VALUE, "UTF-8");

        //使用java8 (默认是使用java8)
        final Element targetJava8Property = this.addElement(context, PROPERTY);
        targetJava8Property.setAttribute(NAME, "targetJava8");
        targetJava8Property.setAttribute(VALUE, "true");

        final Element serializablePlugin = this.addElement(context, "plugin");
        serializablePlugin.setAttribute("type", "org.mybatis.generator.plugins.SerializablePlugin");

        final Element tkKeySqlPlugin = this.addElement(context, "plugin");
        tkKeySqlPlugin.setAttribute("type", "org.mybatis.generator.plugins.TkMybatisKeySqlPlugin");

        // 是否成成注释
        if (tkMybatisExportConfig.isUseComment()) {
            final Element commentGenerator = this.addElement(context, "commentGenerator");
            commentGenerator.setAttribute("type", "com.alan344.comment.MyCommentGenerator");
            final Element suppressAllCommentsPro = this.addElement(commentGenerator, PROPERTY);
            suppressAllCommentsPro.setAttribute(NAME, "suppressAllComments");
            suppressAllCommentsPro.setAttribute(VALUE, "false");
            final Element addRemarkCommentsPro = this.addElement(commentGenerator, PROPERTY);
            addRemarkCommentsPro.setAttribute(NAME, "addRemarkComments");
            addRemarkCommentsPro.setAttribute(VALUE, "true");
            final Element suppressDatePro = this.addElement(commentGenerator, PROPERTY);
            suppressDatePro.setAttribute(NAME, "suppressDate");
            suppressDatePro.setAttribute(VALUE, "false");

            final Element authorPro = this.addElement(commentGenerator, PROPERTY);
            authorPro.setAttribute(NAME, "author");
            authorPro.setAttribute(VALUE, generatorConfig.getAuthor());
        }

        // jdbc 连接
        final Element jdbcConnection = this.addElement(context, "jdbcConnection");
        jdbcConnection.setAttribute("driverClass", "com.mysql.cj.jdbc.Driver");
        jdbcConnection.setAttribute("connectionURL", "jdbc:mysql://" + BaseConstants.selectedDateSource.getHost() + "/" + BaseConstants.selectedDateSource.getDatabase() + "?nullCatalogMeansCurrent=true");
        jdbcConnection.setAttribute("userId", BaseConstants.selectedDateSource.getUser());
        jdbcConnection.setAttribute("password", BaseConstants.selectedDateSource.getPassword());

        //默认类型解析器
        if (tkMybatisExportConfig.isUserJava8() && tkMybatisExportConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = this.addElement(context, "javaTypeResolver");

            final Element useJSR310TypesProperty = this.addElement(javaTypeResolver, PROPERTY);
            useJSR310TypesProperty.setAttribute(NAME, "useJSR310Types");
            useJSR310TypesProperty.setAttribute(VALUE, "true");

            final Element forceBigDecimals = this.addElement(javaTypeResolver, PROPERTY);
            forceBigDecimals.setAttribute(NAME, "forceBigDecimals");
            forceBigDecimals.setAttribute(VALUE, "true");
        } else if (tkMybatisExportConfig.isUserJava8()) {
            final Element javaTypeResolver = this.addElement(context, "javaTypeResolver");

            final Element useJSR310TypesProperty = this.addElement(javaTypeResolver, PROPERTY);
            useJSR310TypesProperty.setAttribute(NAME, "useJSR310Types");
            useJSR310TypesProperty.setAttribute(VALUE, "true");
        } else if (tkMybatisExportConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = this.addElement(context, "javaTypeResolver");

            final Element forceBigDecimals = this.addElement(javaTypeResolver, "forceBigDecimals");
            forceBigDecimals.setAttribute(NAME, "forceBigDecimals");
            forceBigDecimals.setAttribute(VALUE, "true");
        }

        // model 位置配置
        final Element javaModelGenerator = this.addElement(context, "javaModelGenerator");
        javaModelGenerator.setAttribute("targetPackage", generatorConfig.getBeanPackage());
        javaModelGenerator.setAttribute("targetProject", generatorConfig.getBeanLocation().replaceAll("\\\\", "/"));

        // xml 位置配置
        if (StringUtils.isNotEmpty(generatorConfig.getMapperXmlLocation())) {
            final Element sqlMapGenerator = this.addElement(context, "sqlMapGenerator");
            sqlMapGenerator.setAttribute("targetPackage", SeparatorConstants.DOT);
            sqlMapGenerator.setAttribute("targetProject", generatorConfig.getMapperXmlLocation().replaceAll("\\\\", "/"));
        }

        // mapper （dao） 位置配置
        final Element javaClientGenerator = this.addElement(context, "javaClientGenerator");
        javaClientGenerator.setAttribute("targetPackage", generatorConfig.getMapperPackage());
        javaClientGenerator.setAttribute("targetProject", generatorConfig.getMapperLocation().replaceAll("\\\\", "/"));
        javaClientGenerator.setAttribute("type", "XMLMAPPER");
        // Mapper 接口
        this.addMapperRootInterfaceIfNecessary(generatorConfig, javaClientGenerator);

        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {

            final Element tableEl = this.addElement(context, "table");
            tableEl.setAttribute("tableName", table.getTableName());

            List<Column> columns = table.getColumns();

            if (table.isReturnInsertId()) {
                final Element generatedKey = this.addElement(tableEl, "generatedKey");
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
                final Element property = this.addElement(tableEl, PROPERTY);
                property.setAttribute(NAME, "jdkSerializable");
                property.setAttribute(VALUE, "true");
            }

            // 是否使用原来的字段名
            if (generatorConfig.isUseActualColumnNames()) {
                tableEl.setAttribute("useActualColumnNames", "true");
            }

            columns.forEach(column -> {
                if (column.isIgnore()) {
                    final Element ignoreColumn = this.addElement(tableEl, "ignoreColumn");
                    ignoreColumn.setAttribute("column", column.getColumnName());
                }

                ColumnOverride columnOverride = column.getColumnOverride();
                if (columnOverride.isNotEmpty()) {
                    final Element columnOverrideEl = this.addElement(tableEl, "columnOverride");
                    columnOverrideEl.setAttribute("column", column.getColumnName());
                    if (StringUtils.isNotEmpty(columnOverride.getProperty())) {
                        columnOverrideEl.setAttribute(PROPERTY, columnOverride.getProperty());
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
