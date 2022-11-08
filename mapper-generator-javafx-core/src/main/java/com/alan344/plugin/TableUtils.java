package com.alan344.plugin;

import com.alan344.utils.StringUtils;
import com.alibaba.fastjson2.JSON;
import com.google.common.base.CaseFormat;
import lombok.Getter;
import lombok.Setter;
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
        if (StringUtils.isNotEmpty(globalIgnoreColumns)) {
            return globalIgnoreColumns.contains(columnName);
        }

        return false;
    }

    /**
     * 获取类 full name
     *
     * @param introspectedTable 表
     * @param packageName       包名
     */
    public static FullyQualifiedJavaType getTypeFullName(IntrospectedTable introspectedTable, String packageName, String suffix) {
        String requestPackage = packageName + "." + getUpperCamel(introspectedTable) + StringUtils.getDefaultIfNull(suffix, "");
        return new FullyQualifiedJavaType(requestPackage);
    }

    /**
     * 首字母大写驼峰
     *
     * @param introspectedTable 类
     */
    public static String getUpperCamel(IntrospectedTable introspectedTable) {
        final String baseRecordType = introspectedTable.getBaseRecordType();
        return baseRecordType.substring(baseRecordType.lastIndexOf(".") + 1);
    }

    /**
     * 首字母小写驼峰
     */
    public static String getLowerCase(String upperCamel) {
        char[] methodName = upperCamel.toCharArray();
        char firstLetter = methodName[0];
        if (65 <= firstLetter && firstLetter <= 90) {
            methodName[0] = (char) (firstLetter + 32);
        }
        return String.valueOf(methodName);
    }

    /**
     * 小写中划线
     */
    public static String getLowerHyphen(String upperCamel) {
        return CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, upperCamel);
    }

    /**
     * 从备注中获取领域
     *
     * @param remarks 备注
     * @return 领域
     */
    public static Domain getDomainFromRemarks(String remarks, boolean enableDomain) {
        if (!enableDomain) {
            return Domain.DOMAIN;
        }
        if (!org.springframework.util.StringUtils.hasText(remarks)) {
            return Domain.DOMAIN;
        }
        final int start = remarks.indexOf("{");
        final int end = remarks.lastIndexOf("}");
        if (start >= 0 && end >= 0) {
            final String substring = remarks.substring(start, end + 1);
            return JSON.parseObject(substring, Domain.class);
        } else {
            return Domain.DOMAIN;
        }
    }

    @Getter
    @Setter
    public static class Domain {

        public static final Domain DOMAIN = new Domain();

        private String d = "";

        private String dd = "";
    }
}
