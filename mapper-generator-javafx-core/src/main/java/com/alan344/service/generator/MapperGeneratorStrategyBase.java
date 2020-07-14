package com.alan344.service.generator;

import com.alan344.bean.GeneratorConfig;
import com.alan344.utils.MyShellCallback;
import com.alan344happyframework.exception.BizException;
import com.alan344happyframework.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.api.MyBatisGenerator;
import org.mybatis.generator.config.Configuration;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.config.xml.ConfigurationParser;
import org.mybatis.generator.exception.InvalidConfigurationException;
import org.mybatis.generator.exception.XMLParserException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author AlanSun
 * @date 2020/6/5 16:54
 */
@Slf4j
public abstract class MapperGeneratorStrategyBase implements MapperGeneratorStrategy {
    protected DocumentBuilder documentBuilder;

    protected Document doc;

    protected static final String PROPERTY = "property";
    protected static final String NAME = "name";
    protected static final String VALUE = "value";

    {
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        try {
            documentBuilder = documentBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            log.error("初始化错误", e);
        }
    }

    /**
     * 添加 el
     *
     * @param parent    父el
     * @param childName 子el名称
     * @return 子el
     */
    protected Element addElement(Element parent, String childName) {
        final Element element = doc.createElement(childName);
        if (parent != null) {
            parent.appendChild(element);
        }

        return element;
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
    protected void checkAndGeneratorDir(GeneratorConfig generatorConfig) {
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
    protected void generateMyBatis3(Document document, GeneratorConfig generatorConfig) {
        List<String> warnings = new ArrayList<>();
        ConfigurationParser cp = new ConfigurationParser(warnings);
        try {
            Configuration config = cp.parseConfiguration(document);

            MyShellCallback shellCallback = new MyShellCallback(true, generatorConfig.isUseMerge());

            MyBatisGenerator myBatisGenerator = new MyBatisGenerator(config, shellCallback, warnings);
            myBatisGenerator.generate(null, null, null);
        } catch (InvalidConfigurationException | XMLParserException | InterruptedException | IOException | SQLException e) {
            log.error("生成mapper error", e);
            throw new BizException("导出失败");
        }

        if (!warnings.isEmpty()) {
            warnings.forEach(log::warn);
        }
    }

    /**
     * 填充 Mapper 通用接口
     */
    protected void addMapperRootInterfaceIfNecessary(GeneratorConfig generatorConfig, Element javaClientGenerator) {
        final String mapperRootInterface = generatorConfig.getMapperRootInterface();
        if(StringUtils.isNotEmpty(mapperRootInterface)){
            final Element rootInterface = this.addElement(javaClientGenerator, PROPERTY);
            rootInterface.setAttribute(NAME, PropertyRegistry.ANY_ROOT_INTERFACE);
            rootInterface.setAttribute(VALUE, mapperRootInterface);
        }
    }
}
