package com.alan344.service.generator;

import cn.hutool.core.util.StrUtil;
import com.alan344.bean.Column;
import com.alan344.bean.ColumnOverride;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.constants.TablePropertyConstants;
import com.alan344.constants.enums.FileWriteModeEnum;
import com.alan344.mybatisplugin.DomainPlugin;
import com.alan344.mybatisplugin.ExtraFileCustomTemplateGeneratorPlugin;
import com.alan344.mybatisplugin.ExtraFileJPAlGeneratorPlugin;
import com.alan344.mybatisplugin.ExtraFileModelGeneratorPlugin;
import com.alan344.mybatisplugin.JpaAnnotationPlugin;
import com.alan344.mybatisplugin.MybatisGeneratorPlugin;
import com.alan344.mybatisplugin.PluginUtils;
import com.alan344.mybatisplugin.SerializablePlugin;
import com.alan344.mybatisplugin.ValidationAnnotationPlugin;
import com.alan344.utils.MyShellCallback;
import com.alan344.utils.StringUtils;
import com.alan344.utils.Toast;
import com.github.uinio.mybatis.LombokPlugin;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.api.MyBatisGenerator;
import org.mybatis.generator.config.Configuration;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.config.xml.ConfigurationParser;
import org.mybatis.generator.exception.InvalidConfigurationException;
import org.mybatis.generator.internal.util.JavaBeansUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static com.alan344.mybatisplugin.PluginUtils.Domain.DOMAIN;

/**
 * @author AlanSun
 * @date 2020/6/5 16:54
 */
@Getter
@Slf4j
public abstract class MapperGeneratorStrategyBase implements MapperGeneratorStrategy {
    private static DocumentBuilder documentBuilder;

    private final MybatisExportConfig.ExportConfig exportConfig;

    private Document doc;

    private Element context;

    private final List<MybatisPluginConfig> mybatisPluginConfigs;

    static {
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        try {
            documentBuilder = documentBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            log.error("初始化错误", e);
        }
    }

    public MapperGeneratorStrategyBase(MybatisExportConfig.ExportConfig exportConfig,
                                       List<MybatisPluginConfig> mybatisPluginConfigs) {
        this.exportConfig = exportConfig;
        this.mybatisPluginConfigs = mybatisPluginConfigs;
    }

    @Override
    public void generator(MybatisExportConfig mybatisExportConfig) {
        doc = documentBuilder.newDocument();
        final Element root = doc.createElement("generatorConfiguration");
        doc.appendChild(root);

        GeneratorUtils generatorUtils = new GeneratorUtils(this);

        context = generatorUtils.addElement(root, "context");
        context.setAttribute("id", "context1");
        context.setAttribute("targetRuntime", exportConfig.getTargetName().name());

        // 添加 context 属性
        this.addContextProperty(generatorUtils);

        // 添加插件
        this.addPlugin(generatorUtils, mybatisExportConfig);

        // 注释
        this.addComment(generatorUtils, mybatisExportConfig);

        // jdbc 连接
        this.addJdbcConfig(generatorUtils);

        // 默认类型解析器
        this.addResolver(generatorUtils);

        // 添加 model，mapper,xml 地址
        this.addGeneratorLocation(generatorUtils, mybatisExportConfig);

        // 添加 table 配置
        this.addTableConfig(generatorUtils, mybatisExportConfig);

        // 判断文件夹是否存在，如果不存在则进行创建
        this.checkAndGeneratorDir(mybatisExportConfig);

        // 执行创建
        this.generateMyBatis3(doc, mybatisExportConfig);
    }

    /**
     * 添加 context 属性
     */
    protected void addContextProperty(GeneratorUtils generatorUtils) {
        // 指定编码
        generatorUtils.addProperty(true, context, PropertyRegistry.CONTEXT_JAVA_FILE_ENCODING,
                StandardCharsets.UTF_8.toString());
    }

    /**
     * 添加插件
     */
    protected void addPlugin(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        // 添加自定义插件
        Optional.ofNullable(this.mybatisPluginConfigs).ifPresent(mybatisPluginConfigs -> {
            for (MybatisPluginConfig mybatisPluginConfig : mybatisPluginConfigs) {
                generatorUtils.addPlugin(mybatisPluginConfig.getClassName());
            }
        });
        // 添加序列化接口插件
        generatorUtils.addPlugin(SerializablePlugin.class.getName());
        final MybatisExportConfig.MybatisOfficialExportConfig exportConfig =
                mybatisExportConfig.getMybatisOfficialExportConfig();

        // lombok 插件
        final Element lombok = generatorUtils.addPlugin(LombokPlugin.class.getName());
        generatorUtils.addProperty(exportConfig.isUseLombokGetSet(), lombok, "getter", "true");
        generatorUtils.addProperty(exportConfig.isUseLombokGetSet(), lombok, "setter", "true");
        generatorUtils.addProperty(exportConfig.isUseLombokBuilder(), lombok, "builder", "true");

        if (exportConfig.isUseValidationAnnotation()) {
            generatorUtils.addPlugin(ValidationAnnotationPlugin.class.getName());
        }


        if (exportConfig.isUseJpaAnnotation()) {
            generatorUtils.addPlugin(JpaAnnotationPlugin.class.getName());
        }

        if (mybatisExportConfig.isExportExtraFile()) {
            // 额外 model 生成插件
            generatorUtils.addPlugin(ExtraFileModelGeneratorPlugin.class.getName());
            // JPA
            generatorUtils.addPlugin(ExtraFileJPAlGeneratorPlugin.class.getName());
            // 额外的模板文件生成插件
            generatorUtils.addPlugin(ExtraFileCustomTemplateGeneratorPlugin.class.getName());
        }

        // 用于控制是否生成对应的 mybatis 文件
        generatorUtils.addPlugin(MybatisGeneratorPlugin.class.getName());

        // 修改 domain 类名
        generatorUtils.addPlugin(DomainPlugin.class.getName());
    }

    /**
     * 添加注释
     *
     * @param mybatisExportConfig 配置信息
     */
    protected void addComment(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        // 是否成成注释
        final Element commentGenerator = generatorUtils.addElement(context, "commentGenerator");
        commentGenerator.setAttribute("type", MyCommentGenerator.class.getName());
        generatorUtils.addProperty(true,
                commentGenerator, PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT, "yyyy-MM-dd HH:mm:ss");
        generatorUtils.addProperty(true, commentGenerator, "author", mybatisExportConfig.getAuthor());
    }

    /**
     * 添加 jdbc 连接信息
     */
    private void addJdbcConfig(GeneratorUtils generatorUtils) {
        final DataSource selectedDateSource = BaseConstants.selectedDateSource;
        final Element jdbcConnection = generatorUtils.addElement(context, "jdbcConnection");
        jdbcConnection.setAttribute("driverClass", selectedDateSource.getDriveName());
        jdbcConnection.setAttribute("connectionURL", selectedDateSource.getUrl());
        jdbcConnection.setAttribute("userId", selectedDateSource.getUser());
        jdbcConnection.setAttribute("password", selectedDateSource.getPassword());
        generatorUtils.addProperty(true, jdbcConnection, "useInformationSchema", "true");
        generatorUtils.addProperty(true, jdbcConnection, "remarks", "true");
    }

    /**
     * 添加解析器
     */
    protected void addResolver(GeneratorUtils generatorUtils) {
        if (exportConfig.isUserJava8() && exportConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = generatorUtils.addElement(context, "javaTypeResolver");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_USE_JSR310_TYPES, "true");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_FORCE_BIG_DECIMALS,
                    "true");
        } else if (exportConfig.isUserJava8()) {
            final Element javaTypeResolver = generatorUtils.addElement(context, "javaTypeResolver");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_USE_JSR310_TYPES, "true");
        } else if (exportConfig.isUseBigDecimal()) {
            final Element javaTypeResolver = generatorUtils.addElement(context, "javaTypeResolver");
            generatorUtils.addProperty(true, javaTypeResolver, PropertyRegistry.TYPE_RESOLVER_FORCE_BIG_DECIMALS,
                    "true");
        }
    }

    /**
     * 添加 model，mapper,xml 地址
     *
     * @param mybatisExportConfig 配置信息
     */
    private void addGeneratorLocation(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        // model 位置配置
        this.addModelGenerator(generatorUtils, mybatisExportConfig);

        // xml 位置配置
        this.addXmlGenerator(generatorUtils, mybatisExportConfig);

        // mapper （dao） 位置配置
        this.addClientGenerator(generatorUtils, mybatisExportConfig);
    }

    /**
     * 添加 model 生成配置
     *
     * @param generatorUtils      工具
     * @param mybatisExportConfig 配置信息
     */
    protected void addModelGenerator(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        // model 位置配置
        final Element javaModelGenerator = generatorUtils.addElement(context, "javaModelGenerator");
        javaModelGenerator.setAttribute("targetPackage",
                StringUtils.getDefaultIfNull(mybatisExportConfig.getBeanPackage(), "."));
        javaModelGenerator.setAttribute("targetProject", (StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectDir(),
                StrUtil.SLASH) + mybatisExportConfig.getBeanLocation()).replace(StrUtil.BACKSLASH, StrUtil.SLASH));
    }

    /**
     * 添加 Mapper 生成配置
     *
     * @param generatorUtils      工具
     * @param mybatisExportConfig 配置信息
     */
    protected void addXmlGenerator(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        // xml 位置配置
        final Element sqlMapGenerator = generatorUtils.addElement(context, "sqlMapGenerator");
        sqlMapGenerator.setAttribute("targetPackage", ".");
        sqlMapGenerator.setAttribute("targetProject", (StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectDir(),
                "/") + mybatisExportConfig.getMapperXmlLocation()).replace(StrUtil.BACKSLASH, StrUtil.SLASH));
    }

    /**
     * 添加 XML 生成配置
     *
     * @param generatorUtils      工具
     * @param mybatisExportConfig 配置信息
     */
    protected void addClientGenerator(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        // mapper （dao） 位置配置
        final Element javaClientGenerator = generatorUtils.addElement(context, "javaClientGenerator");
        javaClientGenerator.setAttribute("targetPackage",
                StringUtils.getDefaultIfNull(mybatisExportConfig.getMapperPackage(), "."));
        javaClientGenerator.setAttribute("targetProject", (StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectDir()
                , StrUtil.SLASH) + mybatisExportConfig.getMapperLocation()).replace(StrUtil.BACKSLASH, StrUtil.SLASH));
        if (null != mybatisExportConfig.getMybatisOfficialExportConfig().getJavaClientType()) {
            javaClientGenerator.setAttribute("type",
                    mybatisExportConfig.getMybatisOfficialExportConfig().getJavaClientType().name());
        }
        // Mapper 接口
        final String mapperRootInterface = mybatisExportConfig.getMapperRootInterface();
        generatorUtils.addProperty(StringUtils.isNotEmpty(mapperRootInterface), javaClientGenerator,
                PropertyRegistry.ANY_ROOT_INTERFACE, mapperRootInterface);
    }

    /**
     * 添加 table 配置信息
     *
     * @param mybatisExportConfig 配置信息
     */
    protected void addTableConfig(GeneratorUtils generatorUtils, MybatisExportConfig mybatisExportConfig) {
        Collection<Table> tables = BaseConstants.selectedTableNameTableMap.values();
        for (Table table : tables) {

            final Element tableEl = generatorUtils.addElement(context, "table");
            tableEl.setAttribute("tableName", table.getTableName());

            if (BaseConstants.selectedDateSource.getScheme() != null) {
                tableEl.setAttribute("catalog", BaseConstants.selectedDateSource.getScheme());
            }

            // 处理 package 领域
            if (exportConfig.isEnableDomain()) {
                this.domainHandle(table, "domainObjectName", tableEl);
                this.domainHandle(table, "mapperName", tableEl);
                this.domainHandle(table, "sqlProviderName", tableEl);
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
            generatorUtils.setAttribute(mybatisExportConfig.isUseActualColumnNames(), tableEl, "useActualColumnNames"
                    , "true");

            List<Column> columns = table.getColumns();

            if (table.isReturnInsertId()) {
                final Element generatedKey = generatorUtils.addElement(tableEl, "generatedKey");
                generatedKey.setAttribute("column", columns.get(0).getColumnName());
                generatedKey.setAttribute("sqlStatement", "JDBC");
            }

            final String globalIgnoreField = mybatisExportConfig.getGlobalIgnoreField();
            if (StringUtils.isNotEmpty(globalIgnoreField)) {
                final String[] globalIgnoreArr = globalIgnoreField.split(",");
                for (String globalIgnore : globalIgnoreArr) {
                    final Element ignoreColumn = generatorUtils.addElement(tableEl, "ignoreColumn");
                    ignoreColumn.setAttribute("column", globalIgnore);
                }
            }

            // 添加默认属性
            generatorUtils.addProperty(table.isJdkSerializable(), tableEl, TablePropertyConstants.JDK_SERIALIZABLE, "true");
            generatorUtils.addProperty(StringUtils.isNotEmpty(mybatisExportConfig.getModelRootClass()), tableEl,
                    PropertyRegistry.ANY_ROOT_CLASS, mybatisExportConfig.getModelRootClass());

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

                    generatorUtils.setAttribute(StringUtils.isNotEmpty(columnOverride.getProperty()),
                            columnOverrideEl, GeneratorUtils.PROPERTY, columnOverride.getProperty());
                    generatorUtils.setAttribute(StringUtils.isNotEmpty(columnOverride.getJavaType()),
                            columnOverrideEl, "javaType", columnOverride.getJavaType());
                    generatorUtils.setAttribute(StringUtils.isNotEmpty(columnOverride.getTypeHandler()),
                            columnOverrideEl, "typeHandler", columnOverride.getTypeHandler());
                    generatorUtils.setAttribute(columnOverride.isGeneratedAlways(), columnOverrideEl,
                            "isGeneratedAlways", String.valueOf(columnOverride.isGeneratedAlways()));
                    generatorUtils.setAttribute(columnOverride.isDelimitedColumnName(), columnOverrideEl,
                            "delimitedColumnName", String.valueOf(columnOverride.isDelimitedColumnName()));
                }

                this.addTableProperty();
            }
        }
    }

    protected void addTableProperty() {
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
    private void checkAndGeneratorDir(MybatisExportConfig mybatisExportConfig) {
        final String prefix = StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectDir(), StrUtil.SLASH);
        if (StringUtils.isNotEmpty(mybatisExportConfig.getBeanLocation())) {
            File beanFile = new File(prefix + mybatisExportConfig.getBeanLocation());
            if (!beanFile.exists() && !beanFile.mkdirs()) {
                log.error("创建bean文件夹：{} 失败", prefix + mybatisExportConfig.getBeanLocation());
            }
        }

        if (StringUtils.isNotEmpty(mybatisExportConfig.getMapperLocation())) {
            File mapperFile = new File(prefix + mybatisExportConfig.getMapperLocation());
            if (!mapperFile.exists() && !mapperFile.mkdirs()) {
                log.error("创建mapper文件夹：{} 失败", prefix + mybatisExportConfig.getMapperLocation());
            }
        }

        if (!mybatisExportConfig.getMapperXmlLocation().contains("${") && StringUtils.isNotEmpty(mybatisExportConfig.getMapperXmlLocation())) {
            File xmlFile = new File(prefix + mybatisExportConfig.getMapperXmlLocation());
            if (!xmlFile.exists() && !xmlFile.mkdirs()) {
                log.error("创建xml文件夹：{} 失败", prefix + mybatisExportConfig.getMapperXmlLocation());
            }
        }
    }

    /**
     * 调用 mybatis-generator
     *
     * @param document xml
     */
    private void generateMyBatis3(Document document, MybatisExportConfig mybatisExportConfig) {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        Configuration config;
        try {
            try {
                final Method parseMyBatisGeneratorConfiguration = ConfigurationParser.class.getDeclaredMethod(
                        "parseMyBatisGeneratorConfiguration", Element.class);
                parseMyBatisGeneratorConfiguration.setAccessible(true);
                config = ((Configuration) parseMyBatisGeneratorConfiguration.invoke(cp, document.getDocumentElement()));
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            MyShellCallback shellCallback = new MyShellCallback(false, true);
            if (mybatisExportConfig.getWriteMode().equals(FileWriteModeEnum.OVERWRITE)) {
                shellCallback = new MyShellCallback(true, false);
            }

            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null);
        } catch (InvalidConfigurationException | InterruptedException | IOException | SQLException e) {
            log.error("生成mapper error", e);
            if (e instanceof final InvalidConfigurationException invalidConfigurationException) {
                final List<String> errors = invalidConfigurationException.getErrors();
                Toast.makeTextDefault(NodeConstants.primaryStage, errors.get(0));
                return;
            }
            throw new RuntimeException("导出失败");
        }

        if (!warnings.isEmpty()) {
            warnings.forEach(log::warn);
        }
    }

    protected void domainHandle(Table table, String attributeName, Element tableEl) {
        final PluginUtils.Domain domainFromRemarks = PluginUtils.getDomainFromRemarks(table.getRemark(), true);
        if (domainFromRemarks != DOMAIN) {
            tableEl.setAttribute(attributeName,
                    domainFromRemarks.getD() + "." + JavaBeansUtil.getCamelCaseString(table.getTableName(), true));
        }
    }
}
