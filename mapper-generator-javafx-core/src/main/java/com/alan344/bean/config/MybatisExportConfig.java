package com.alan344.bean.config;

import lombok.Getter;
import lombok.Setter;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Objects;

/**
 * @author AlanSun
 * @date 2019/8/13 16:20
 */
@Getter
@Setter
public class MybatisExportConfig implements Cloneable {
    /**
     * 配置的名称
     */
    private String configName;
    /**
     * 类中的作者信息
     */
    private String author;
    /**
     * 是否只生成 model
     */
    private boolean modelOnly;
    /**
     * 使用支持 merge
     */
    private boolean useMerge;
    /**
     * bean 的导出地址
     */
    private String beanLocation;
    /**
     * bean 包名
     */
    private String beanPackage;
    /**
     * mapper 导出地址
     */
    private String mapperLocation;
    /**
     * mapperBean 包名
     */
    private String mapperPackage;
    /***
     * xml导出地址
     */
    private String mapperXmlLocation;
    /**
     * 使用原来的字段名
     */
    private boolean useActualColumnNames;
    /**
     * mapper 的统一接口
     */
    private String mapperRootInterface;
    /**
     * 全局的忽略字段
     */
    private String globalIgnoreField;
    /**
     * 选择的哪个tab
     */
    private int selectTab;
    /**
     * 是否导出额外文件
     */
    private boolean isExportExtraFile;
    /**
     * Mybatis-generator 原生配置
     */
    private MybatisOfficialExportConfig mybatisOfficialExportConfig = new MybatisOfficialExportConfig();
    /**
     * model 的父类
     */
    private String modelRootClass;


    //---------------------extra file----------------

    private List<ExtraFileGroupConfig> extraFileGroupConfigs;

    /**
     * 自定义属性
     */
    private LinkedHashMap<String, String> customProperties;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        MybatisExportConfig that = (MybatisExportConfig) o;
        return Objects.equals(configName, that.configName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(configName);
    }

    @Override
    public MybatisExportConfig clone() {
        try {
            MybatisExportConfig clone = (MybatisExportConfig) super.clone();
            clone.setMybatisOfficialExportConfig(mybatisOfficialExportConfig.clone());
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    public interface ExportConfig {
        boolean isUserJava8();

        boolean isUseBigDecimal();

        boolean isUseLombokGetSet();

        boolean isUseComment();

        String getTargetName();

        /**
         * 是否开启领域，开启后如果数据库表注释存在类似 {"d":"","dd":""} {@link com.alan344.plugin.PluginUtils.Domain}
         */
        boolean isEnableDomain();
    }

    /**
     * Mybatis-generator 原生
     */
    @Getter
    @Setter
    public static class MybatisOfficialExportConfig implements ExportConfig, Cloneable {
        /**
         * Mybatis3，MyBatis3Simple，MyBatis3DynamicSql
         */
        private String targetName;
        /**
         * 是否使用java8
         */
        private boolean userJava8 = true;
        /**
         * 是否支持 BigDecimal
         * <p>
         * 所有 number 都是用 BigDecimal
         */
        private boolean useBigDecimal;
        /**
         * 使用支持 lombok
         */
        private boolean useLombokGetSet;
        /**
         * 是否使用 lombok builder
         */
        private boolean useLombokBuilder;
        /**
         * 使用注释
         */
        private boolean useComment = true;
        /**
         * 是否开启领域，开启后如果数据库表注释存在类似 {"d":"","dd":""} {@link com.alan344.plugin.PluginUtils.Domain}
         */
        private boolean enableDomain = false;

        @Override
        public MybatisOfficialExportConfig clone() {
            try {
                MybatisOfficialExportConfig clone = (MybatisOfficialExportConfig) super.clone();
                // TODO: copy mutable state here, so the clone can't change the internals of the original
                return clone;
            } catch (CloneNotSupportedException e) {
                throw new AssertionError();
            }
        }
    }
}
