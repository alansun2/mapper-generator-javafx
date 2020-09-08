package com.alan344.service.generator;

import com.alan344.bean.Column;
import com.alan344.bean.ColumnOverride;
import com.alan344.bean.GeneratorConfig;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.StageConstants;
import com.alan344.utils.MyShellCallback;
import com.alan344.utils.Toast;
import com.alan344happyframework.constants.SeparatorConstants;
import com.alan344happyframework.exception.BizException;
import com.alan344happyframework.util.StringUtils;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.api.MyBatisGenerator;
import org.mybatis.generator.config.Configuration;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.config.xml.ConfigurationParser;
import org.mybatis.generator.exception.InvalidConfigurationException;
import org.mybatis.generator.exception.XMLParserException;
import org.mybatis.generator.my.comment.MyCommentGenerator;
import org.mybatis.generator.plugins.SerializablePlugin;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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
 * @date 2020/6/5 16:54
 */
@Getter
@Slf4j
public abstract class MapperGeneratorStrategyBase implements MapperGeneratorStrategy {
    private static DocumentBuilder documentBuilder;

    private final GeneratorConfig.ExportConfig exportConfig;

    private Document doc;

    private Element context;

    static {
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        try {
            documentBuilder = documentBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            log.error("初始化错误", e);
        }
    }

    public MapperGeneratorStrategyBase(GeneratorConfig.ExportConfig exportConfig) {
        this.exportConfig = exportConfig;
    }

    @Override
    public void generator(GeneratorConfig generatorConfig) {
        doc = documentBuilder.newDocument();
        final Element root = doc.createElement("generatorConfiguration");
        doc.appendChild(root);

        GeneratorUtils generatorUtils = new GeneratorUtils(this);

        context = generatorUtils.addElement(root, "context");
        context.setAttribute("id", "context1");
        context.setAttribute("targetRuntime", exportConfig.getTargetName());

        // 添加 context 属性
        this.addContextProperty(generatorUtils);

        //添加插件
        this.addPlugin(generatorUtils);

        // 注释
        this.addComment(generatorUtils, generatorConfig);

        // jdbc 连接
        this.addJdbcConfig(generatorUtils);

        // 默认类型解析器
        this.addResolver(generatorUtils);

        // 添加 model，mapper,xml 地址
        this.addGeneratorLocation(generatorUtils, generatorConfig);

        // 添加 table 配置
        this.addTableConfig(generatorUtils, generatorConfig);

        // 判断文件夹是否存在，如果不存在则进行创建
        this.checkAndGeneratorDir(generatorConfig);
        // 执行创建
        this.generateMyBatis3(doc, generatorConfig);
    }

    /**
     * 添加 context 属性
     */
    protected void addContextProperty(GeneratorUtils generatorUtils) {
        // 指定编码
        generatorUtils.addProperty(true, context, PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING, "UTF-8");

        // 使用java8 (默认是使用java8)
        generatorUtils.addProperty(true, context, PropertyRegistry.CONTEXT_TARGET_JAVA8, exportConfig.isUserJava8() + "");
    }

    /**
     * 添加插件
     */
    protected void addPlugin(GeneratorUtils generatorUtils) {
        // 序列化插件
        generatorUtils.addPlugin(SerializablePlugin.class.getName());
    }

    /**
     * 添加注释
     *
     * @param generatorConfig 配置信息
     */
    protected void addComment(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
        // 是否成成注释
        final Element commentGenerator = generatorUtils.addElement(context, "commentGenerator");
        commentGenerator.setAttribute("type", MyCommentGenerator.class.getName());
        generatorUtils.addProperty(true, commentGenerator, PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS, exportConfig.isUseComment() + "");
        generatorUtils.addProperty(true, commentGenerator, "author", generatorConfig.getAuthor());
        generatorUtils.addProperty(true, commentGenerator, "supportSwagger", exportConfig.isUseSwagger() + "");
    }

    /**
     * 添加 jdbc 连接信息
     */
    private void addJdbcConfig(GeneratorUtils generatorUtils) {
        final Element jdbcConnection = generatorUtils.addElement(context, "jdbcConnection");
        jdbcConnection.setAttribute("driverClass", "com.mysql.cj.jdbc.Driver");
        jdbcConnection.setAttribute("connectionURL", "jdbc:mysql://" + BaseConstants.selectedDateSource.getHost() + "/" + BaseConstants.selectedDateSource.getDatabase() + "?nullCatalogMeansCurrent=true");
        jdbcConnection.setAttribute("userId", BaseConstants.selectedDateSource.getUser());
        jdbcConnection.setAttribute("password", BaseConstants.selectedDateSource.getPassword());
    }

    /**
     * 添加解析器
     */
    protected void addResolver(GeneratorUtils generatorUtils) {
        if (exportConfig.isUserJava8() && exportConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = generatorUtils.addElement(context, "javaTypeResolver");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_USE_JSR310_TYPES, "true");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_FORCE_BIG_DECIMALS, "true");
        } else if (exportConfig.isUserJava8()) {
            final Element javaTypeResolver = generatorUtils.addElement(context, "javaTypeResolver");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_USE_JSR310_TYPES, "true");
        } else if (exportConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = generatorUtils.addElement(context, "javaTypeResolver");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_FORCE_BIG_DECIMALS, "true");
        }
    }

    /**
     * 添加 model，mapper,xml 地址
     *
     * @param generatorConfig 配置信息
     */
    private void addGeneratorLocation(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
        // model 位置配置
        this.addModelGenerator(generatorUtils, generatorConfig);

        // xml 位置配置
        this.addMapperGenerator(generatorUtils, generatorConfig);

        // mapper （dao） 位置配置
        this.addXmlGenerator(generatorUtils, generatorConfig);
    }

    /**
     * 添加 model 生成配置
     *
     * @param generatorUtils  工具
     * @param generatorConfig 配置信息
     */
    protected void addModelGenerator(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
        // model 位置配置
        final Element javaModelGenerator = generatorUtils.addElement(context, "javaModelGenerator");
        javaModelGenerator.setAttribute("targetPackage", generatorConfig.getBeanPackage());
        javaModelGenerator.setAttribute("targetProject", generatorConfig.getBeanLocation().replaceAll("\\\\", "/"));
    }

    /**
     * 添加 Mapper 生成配置
     *
     * @param generatorUtils  工具
     * @param generatorConfig 配置信息
     */
    protected void addMapperGenerator(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
        // xml 位置配置
        if (StringUtils.isNotEmpty(generatorConfig.getMapperXmlLocation())) {
            final Element sqlMapGenerator = generatorUtils.addElement(context, "sqlMapGenerator");
            sqlMapGenerator.setAttribute("targetPackage", SeparatorConstants.DOT);
            sqlMapGenerator.setAttribute("targetProject", generatorConfig.getMapperXmlLocation().replaceAll("\\\\", "/"));
        }
    }

    /**
     * 添加 XML 生成配置
     *
     * @param generatorUtils  工具
     * @param generatorConfig 配置信息
     */
    protected void addXmlGenerator(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
        // mapper （dao） 位置配置
        if (StringUtils.isNotEmpty(generatorConfig.getMapperPackage()) && StringUtils.isNotEmpty(generatorConfig.getMapperLocation())) {
            final Element javaClientGenerator = generatorUtils.addElement(context, "javaClientGenerator");
            javaClientGenerator.setAttribute("targetPackage", generatorConfig.getMapperPackage());
            javaClientGenerator.setAttribute("targetProject", generatorConfig.getMapperLocation().replaceAll("\\\\", "/"));
            javaClientGenerator.setAttribute("type", "XMLMAPPER");
            // Mapper 接口
            final String mapperRootInterface = generatorConfig.getMapperRootInterface();
            generatorUtils.addProperty(StringUtils.isNotEmpty(mapperRootInterface), javaClientGenerator, PropertyRegistry.ANY_ROOT_INTERFACE, mapperRootInterface);
        }
    }

    /**
     * 添加 table 配置信息
     *
     * @param generatorConfig 配置信息
     */
    protected void addTableConfig(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {

            final Element tableEl = generatorUtils.addElement(context, "table");
            tableEl.setAttribute("tableName", table.getTableName());

            List<Column> columns = table.getColumns();

            if (table.isReturnInsertId()) {
                final Element generatedKey = generatorUtils.addElement(tableEl, "generatedKey");
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

            // 是否使用原来的字段名
            if (generatorConfig.isUseActualColumnNames()) {
                tableEl.setAttribute("useActualColumnNames", "true");
            }

            for (Column column : columns) {
                if (column.isIgnore()) {
                    final Element ignoreColumn = generatorUtils.addElement(tableEl, "ignoreColumn");
                    ignoreColumn.setAttribute("column", column.getColumnName());
                    continue;
                }

                ColumnOverride columnOverride = column.getColumnOverride();
                if (columnOverride.isNotEmpty()) {
                    final Element columnOverrideEl = generatorUtils.addElement(tableEl, "columnOverride");
                    columnOverrideEl.setAttribute("column", column.getColumnName());
                    if (StringUtils.isNotEmpty(columnOverride.getProperty())) {
                        columnOverrideEl.setAttribute(GeneratorUtils.PROPERTY, columnOverride.getProperty());
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

                // 添加属性
                generatorUtils.addProperty(table.isJdkSerializable(), tableEl, "jdkSerializable", "true");
                generatorUtils.addProperty(generatorConfig.isModelOnly(), tableEl, PropertyRegistry.TABLE_MODEL_ONLY, "true");
                this.addTableProperty(generatorUtils, generatorConfig);
            }
        }
    }

    protected void addTableProperty(GeneratorUtils generatorUtils, GeneratorConfig generatorConfig) {
    }

    /**
     * 设置 checkBox
     */
    protected void checkBoxSelected(String name, Element table, boolean flag) {
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
        if (StringUtils.isNotEmpty(generatorConfig.getBeanLocation())) {
            File beanFile = new File(generatorConfig.getBeanLocation());
            if (!beanFile.exists() && !beanFile.mkdirs()) {
                log.error("创建bean文件夹：{} 失败", generatorConfig.getBeanLocation());
            }
        }

        if (StringUtils.isNotEmpty(generatorConfig.getMapperLocation())) {
            File mapperFile = new File(generatorConfig.getMapperLocation());
            if (!mapperFile.exists() && !mapperFile.mkdirs()) {
                log.error("创建mapper文件夹：{} 失败", generatorConfig.getMapperLocation());
            }
        }

        if (StringUtils.isNotEmpty(generatorConfig.getMapperXmlLocation())) {
            File xmlFile = new File(generatorConfig.getMapperXmlLocation());
            if (!xmlFile.exists() && !xmlFile.mkdirs()) {
                log.error("创建xml文件夹：{} 失败", generatorConfig.getMapperXmlLocation());
            }
        }
    }

    /**
     * 调用 mybatis-generator
     *
     * @param document xml
     */
    private void generateMyBatis3(Document document, GeneratorConfig generatorConfig) {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        try {
            Configuration config = cp.parseConfiguration(document);

            MyShellCallback shellCallback = new MyShellCallback(true, generatorConfig.isUseMerge());

            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null);
        } catch (InvalidConfigurationException | XMLParserException | InterruptedException | IOException | SQLException e) {
            log.error("生成mapper error", e);
            if (e instanceof InvalidConfigurationException) {
                final InvalidConfigurationException invalidConfigurationException = (InvalidConfigurationException) e;
                final List<String> errors = invalidConfigurationException.getErrors();
                Toast.makeTextDefault(StageConstants.configStage, errors.get(0));
                return;
            }
            throw new BizException("导出失败");
        }

        if (!warnings.isEmpty()) {
            warnings.forEach(log::warn);
        }
    }
}
