package com.alan344.utils;

import com.alan344.bean.ServiceConfig;
import com.alan344happyframework.constants.SeparatorConstants;
import org.apache.commons.lang3.StringUtils;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.dom.java.FullyQualifiedJavaType;
import org.mybatis.generator.config.Context;
import org.mybatis.generator.config.PropertyRegistry;

import java.util.Properties;

/**
 * @author AlanSun
 * @date 2020/12/3 15:33
 */
public class TableUtils {

    public static String getRootClass(IntrospectedTable introspectedTable, Context context) {
        String rootClass = introspectedTable.getTableConfigurationProperty(PropertyRegistry.ANY_ROOT_CLASS);
        if (rootClass == null) {
            Properties properties = context.getJavaModelGeneratorConfiguration().getProperties();
            rootClass = properties.getProperty(PropertyRegistry.ANY_ROOT_CLASS);
        }

        return rootClass;
    }

    /**
     * 字段是否忽略
     *
     * @param globalIgnoreColumns 全局忽略的字段
     * @param columnName          字段
     * @return true；忽略；false: 不忽略
     */
    public static boolean isFieldIgnore(String globalIgnoreColumns, String columnName) {
        if (StringUtils.isNotBlank(globalIgnoreColumns)) {
            return globalIgnoreColumns.contains(columnName);
        }

        return false;
    }

    /**
     * 获取 request bean
     *
     * @param introspectedTable 表
     * @param serviceConfig     service配置
     */
    public static FullyQualifiedJavaType getRequestBeanType(IntrospectedTable introspectedTable, ServiceConfig serviceConfig) {
        final String baseRecordType = introspectedTable.getBaseRecordType();
        String requestPackage = serviceConfig.getRequestPackage() + SeparatorConstants.DOT + baseRecordType.substring(baseRecordType.lastIndexOf(SeparatorConstants.DOT) + 1) + "Request";
        return new FullyQualifiedJavaType(requestPackage);
    }

    /**
     * 获取原始的 bean 名称
     *
     * @param introspectedTable 类
     */
    public static String getOriginalBeanName(IntrospectedTable introspectedTable) {
        final String baseRecordType = introspectedTable.getBaseRecordType();
        return baseRecordType.substring(baseRecordType.lastIndexOf(SeparatorConstants.DOT) + 1);
    }

    /**
     * 首字母大写
     */
    public static String firstLetterCapital(String originalStr) {
        char[] methodName = originalStr.toCharArray();
        char firstLetter = methodName[0];
        if (97 <= firstLetter && firstLetter <= 122) {
            methodName[0] ^= 32;
        }
        return String.valueOf(methodName);
    }

    /**
     * 首字母小写
     */
    public static String firstLetterDownCase(String originalStr) {
        char[] methodName = originalStr.toCharArray();
        char firstLetter = methodName[0];
        if (97 <= firstLetter && firstLetter <= 122) {
            methodName[0] = (char) (firstLetter + 32);
        }
        return String.valueOf(methodName);
    }
}
