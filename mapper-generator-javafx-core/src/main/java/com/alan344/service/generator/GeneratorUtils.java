package com.alan344.service.generator;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author AlanSun
 * @date 2020/7/15 14:56
 */
public class GeneratorUtils {
    public static final String PROPERTY = "property";
    public static final String NAME = "name";
    public static final String VALUE = "value";

    private final MapperGeneratorStrategyBase mapperGeneratorStrategyBase;

    public GeneratorUtils(MapperGeneratorStrategyBase mapperGeneratorStrategyBase) {
        this.mapperGeneratorStrategyBase = mapperGeneratorStrategyBase;
    }

    /**
     * 添加 el
     *
     * @param parent    父el
     * @param childName 子el名称
     * @return 子el
     */
    protected Element addElement(Element parent, String childName) {
        final Element element = mapperGeneratorStrategyBase.getDoc().createElement(childName);
        if (parent != null) {
            parent.appendChild(element);
        }

        return element;
    }

    /**
     * 添加插件
     *
     * @param pluginName 插件类的全路径
     */
    protected void addPlugin(String pluginName) {
        final Element plugin = this.addElement(mapperGeneratorStrategyBase.getContext(), "plugin");
        plugin.setAttribute("type", pluginName);
    }

    /**
     * 添加属性
     *
     * @param condition 用于判断是否添加属性
     * @param parentEl  表 element
     * @param name      属性名
     * @param value     属性值
     */
    protected void addProperty(boolean condition, Element parentEl, String name, String value) {
        if (condition) {
            final Element property = this.addElement(parentEl, PROPERTY);
            property.setAttribute(NAME, name);
            property.setAttribute(VALUE, value);
        }
    }
}
