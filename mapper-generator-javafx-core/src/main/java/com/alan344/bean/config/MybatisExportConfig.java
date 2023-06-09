package com.alan344.bean.config;

import com.alan344.component.LeftRightLinkageBorderPane;
import com.alan344.constants.enums.FileWriteModeEnum;
import com.alan344.constants.enums.JavaClientTypeEnum;
import com.alan344.constants.enums.LanguageEnum;
import com.alan344.constants.enums.TargetNameEnum;
import com.alibaba.fastjson2.JSON;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Objects;

/**
 * @author AlanSun
 * @date 2019/8/13 16:20
 */
public class MybatisExportConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 配置的名称
     */
    private final SimpleStringProperty configName = new SimpleStringProperty();

    private final SimpleBooleanProperty enable = new SimpleBooleanProperty(false);

    /**
     * 是否是系统内置
     */
    private final SimpleBooleanProperty system = new SimpleBooleanProperty(false);

    /**
     * 类中的作者信息
     */
    private final SimpleStringProperty author = new SimpleStringProperty();

    /**
     * 文件写入模式
     */
    private final SimpleObjectProperty<FileWriteModeEnum> writeMode = new SimpleObjectProperty<>(FileWriteModeEnum.MERGE);

    /**
     * 项目地址
     */
    private final SimpleStringProperty projectDir = new SimpleStringProperty();

    /**
     * 项目名
     */
    private final SimpleStringProperty projectName = new SimpleStringProperty();

    /**
     * 开发语言
     */
    private final SimpleObjectProperty<LanguageEnum> language = new SimpleObjectProperty<>(LanguageEnum.Java);

    /**
     * bean 地址
     */
    private final SimpleStringProperty beanLocation = new SimpleStringProperty();

    /**
     * bean 包名
     */
    private final SimpleStringProperty beanPackage = new SimpleStringProperty();

    /**
     * model 的父类
     */
    private final SimpleStringProperty modelRootClass = new SimpleStringProperty();

    /**
     * 开启 model
     */
    private final SimpleBooleanProperty modelEnable = new SimpleBooleanProperty(true);

    /**
     * mapper 导出地址
     */
    private final SimpleStringProperty mapperLocation = new SimpleStringProperty();

    /**
     * mapperBean 包名
     */
    private final SimpleStringProperty mapperPackage = new SimpleStringProperty();

    /**
     * mapper 的统一接口
     */
    private final SimpleStringProperty mapperRootInterface = new SimpleStringProperty();

    /**
     * 开启 model
     */
    private final SimpleBooleanProperty mapperEnable = new SimpleBooleanProperty(true);

    /***
     * xml导出地址
     */
    private final SimpleStringProperty mapperXmlLocation = new SimpleStringProperty("src/main/resources/mapper");

    /**
     * 开启 model
     */
    private final SimpleBooleanProperty xmlEnable = new SimpleBooleanProperty(true);

    /**
     * 全局的忽略字段
     */
    private final SimpleStringProperty globalIgnoreField = new SimpleStringProperty();

    /**
     * 使用原来的字段名
     */
    private final SimpleBooleanProperty useActualColumnNames = new SimpleBooleanProperty(false);

    /**
     * 选择的哪个tab
     */
    private final SimpleIntegerProperty selectTab = new SimpleIntegerProperty(0);

    /**
     * 是否导出额外文件
     */
    @Getter
    @Setter
    private boolean isExportExtraFile;

    /**
     * Mybatis-generator 原生配置
     */
    @Getter
    @Setter
    private MybatisOfficialExportConfig mybatisOfficialExportConfig = new MybatisOfficialExportConfig();

    //---------------------extra file----------------

    @Getter
    @Setter
    private List<ExtraFileGroupConfig> extraFileGroupConfigs;

    /**
     * mybatis 插件 id 列表
     */
    @Getter
    @Setter
    private List<String> pluginIds;

    /**
     * 自定义属性
     */
    @Getter
    @Setter
    private LinkedHashMap<String, String> customProperties;

    public String getConfigName() {
        return this.configName.getValue();
    }

    public void setConfigName(String configName) {
        this.configName.set(configName);
    }

    public SimpleStringProperty configNameProperty() {
        return this.configName;
    }

    @Override
    public boolean isEnable() {
        return enable.get();
    }

    public SimpleBooleanProperty enableProperty() {
        return enable;
    }

    @Override
    public void setEnable(boolean enable) {
        this.enable.set(enable);
    }

    @Override
    public boolean isSystem() {
        return system.get();
    }

    public SimpleBooleanProperty systemProperty() {
        return system;
    }

    @Override
    public void setSystem(boolean system) {
        this.system.set(system);
    }

    public FileWriteModeEnum getWriteMode() {
        return writeMode.get();
    }

    public SimpleObjectProperty<FileWriteModeEnum> writeModeProperty() {
        return writeMode;
    }

    public void setWriteMode(FileWriteModeEnum writeMode) {
        this.writeMode.set(writeMode);
    }

    public String getAuthor() {
        return author.get();
    }

    public SimpleStringProperty authorProperty() {
        return author;
    }

    public void setAuthor(String author) {
        this.author.set(author);
    }

    public String getProjectDir() {
        return projectDir.get();
    }

    public SimpleStringProperty projectDirProperty() {
        return projectDir;
    }

    public void setProjectDir(String projectDir) {
        this.projectDir.set(projectDir);
    }

    public String getProjectName() {
        return projectName.get();
    }

    public SimpleStringProperty projectNameProperty() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName.set(projectName);
    }

    public LanguageEnum getLanguage() {
        return language.get();
    }

    public SimpleObjectProperty<LanguageEnum> languageProperty() {
        return language;
    }

    public void setLanguage(LanguageEnum language) {
        this.language.set(language);
    }

    public String getBeanLocation() {
        return beanLocation.get();
    }

    public SimpleStringProperty beanLocationProperty() {
        return beanLocation;
    }

    public void setBeanLocation(String beanLocation) {
        this.beanLocation.set(beanLocation);
    }

    public String getBeanPackage() {
        return beanPackage.get();
    }

    public SimpleStringProperty beanPackageProperty() {
        return beanPackage;
    }

    public void setBeanPackage(String beanPackage) {
        this.beanPackage.set(beanPackage);
    }

    public String getMapperLocation() {
        return mapperLocation.get();
    }

    public SimpleStringProperty mapperLocationProperty() {
        return mapperLocation;
    }

    public void setMapperLocation(String mapperLocation) {
        this.mapperLocation.set(mapperLocation);
    }

    public String getMapperPackage() {
        return mapperPackage.get();
    }

    public SimpleStringProperty mapperPackageProperty() {
        return mapperPackage;
    }

    public void setMapperPackage(String mapperPackage) {
        this.mapperPackage.set(mapperPackage);
    }

    public String getMapperXmlLocation() {
        return mapperXmlLocation.get();
    }

    public SimpleStringProperty mapperXmlLocationProperty() {
        return mapperXmlLocation;
    }

    public void setMapperXmlLocation(String mapperXmlLocation) {
        this.mapperXmlLocation.set(mapperXmlLocation);
    }

    public boolean isModelEnable() {
        return modelEnable.get();
    }

    public SimpleBooleanProperty modelEnableProperty() {
        return modelEnable;
    }

    public void setModelEnable(boolean modelEnable) {
        this.modelEnable.set(modelEnable);
    }

    public boolean isMapperEnable() {
        return mapperEnable.get();
    }

    public SimpleBooleanProperty mapperEnableProperty() {
        return mapperEnable;
    }

    public void setMapperEnable(boolean mapperEnable) {
        this.mapperEnable.set(mapperEnable);
    }

    public boolean isXmlEnable() {
        return xmlEnable.get();
    }

    public SimpleBooleanProperty xmlEnableProperty() {
        return xmlEnable;
    }

    public void setXmlEnable(boolean xmlEnable) {
        this.xmlEnable.set(xmlEnable);
    }

    public boolean isUseActualColumnNames() {
        return useActualColumnNames.get();
    }

    public SimpleBooleanProperty useActualColumnNamesProperty() {
        return useActualColumnNames;
    }

    public void setUseActualColumnNames(boolean useActualColumnNames) {
        this.useActualColumnNames.set(useActualColumnNames);
    }

    public String getMapperRootInterface() {
        return mapperRootInterface.get();
    }

    public SimpleStringProperty mapperRootInterfaceProperty() {
        return mapperRootInterface;
    }

    public void setMapperRootInterface(String mapperRootInterface) {
        this.mapperRootInterface.set(mapperRootInterface);
    }

    public String getGlobalIgnoreField() {
        return globalIgnoreField.get();
    }

    public SimpleStringProperty globalIgnoreFieldProperty() {
        return globalIgnoreField;
    }

    public void setGlobalIgnoreField(String globalIgnoreField) {
        this.globalIgnoreField.set(globalIgnoreField);
    }

    public int getSelectTab() {
        return selectTab.get();
    }

    public SimpleIntegerProperty selectTabProperty() {
        return selectTab;
    }

    public void setSelectTab(int selectTab) {
        this.selectTab.set(selectTab);
    }

    public String getModelRootClass() {
        return modelRootClass.get();
    }

    public SimpleStringProperty modelRootClassProperty() {
        return modelRootClass;
    }

    public void setModelRootClass(String modelRootClass) {
        this.modelRootClass.set(modelRootClass);
    }

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
    public String getGroupName() {
        return this.configName.get();
    }

    @Override
    public void setGroupName(String groupName) {
        this.configName.set(groupName);
    }

    @Override
    public Collection getList() {
        return null;
    }

    @Override
    public void setList(Collection list) {

    }

    @Override
    public MybatisExportConfig clone() {
        return JSON.parseObject(JSON.toJSONString(this), MybatisExportConfig.class);
    }

    public interface ExportConfig {
        boolean isUserJava8();

        boolean isUseBigDecimal();

        boolean isUseLombokGetSet();

        boolean isUseComment();

        TargetNameEnum getTargetName();

        JavaClientTypeEnum getJavaClientType();

        /**
         * 是否开启领域，开启后如果数据库表注释存在类似 {"d":"","dd":""} {@link com.alan344.mybatisplugin.PluginUtils.Domain}
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
         * @see TargetNameEnum
         */
        private SimpleObjectProperty<TargetNameEnum> targetName = new SimpleObjectProperty<>(TargetNameEnum.Mybatis3);

        /**
         * java client type
         */
        private SimpleObjectProperty<JavaClientTypeEnum> javaClientType = new SimpleObjectProperty<>();

        /**
         * 是否使用java8
         */
        private SimpleBooleanProperty userJava8 = new SimpleBooleanProperty(true);
        /**
         * 是否支持 BigDecimal
         * <p>
         * 所有 number 都是用 BigDecimal
         */
        private SimpleBooleanProperty useBigDecimal = new SimpleBooleanProperty(false);
        /**
         * 使用支持 lombok
         */
        private SimpleBooleanProperty useLombokGetSet = new SimpleBooleanProperty(false);
        /**
         * 是否使用 lombok builder
         */
        private SimpleBooleanProperty useLombokBuilder = new SimpleBooleanProperty(false);
        /**
         * 使用注释
         */
        private SimpleBooleanProperty useComment = new SimpleBooleanProperty(true);
        /**
         * 是否开启领域，开启后如果数据库表注释存在类似 {"d":"","dd":""} {@link com.alan344.mybatisplugin.PluginUtils.Domain}
         */
        private SimpleBooleanProperty enableDomain = new SimpleBooleanProperty(false);

        @Override
        public TargetNameEnum getTargetName() {
            return targetName.get();
        }

        public SimpleObjectProperty<TargetNameEnum> targetNameProperty() {
            return targetName;
        }

        public void setTargetName(TargetNameEnum targetName) {
            this.targetName.set(targetName);
        }

        @Override
        public boolean isUserJava8() {
            return userJava8.get();
        }

        public SimpleBooleanProperty userJava8Property() {
            return userJava8;
        }

        public void setUserJava8(boolean userJava8) {
            this.userJava8.set(userJava8);
        }

        @Override
        public boolean isUseBigDecimal() {
            return useBigDecimal.get();
        }

        public SimpleBooleanProperty useBigDecimalProperty() {
            return useBigDecimal;
        }

        public void setUseBigDecimal(boolean useBigDecimal) {
            this.useBigDecimal.set(useBigDecimal);
        }

        @Override
        public boolean isUseLombokGetSet() {
            return useLombokGetSet.get();
        }

        public SimpleBooleanProperty useLombokGetSetProperty() {
            return useLombokGetSet;
        }

        public void setUseLombokGetSet(boolean useLombokGetSet) {
            this.useLombokGetSet.set(useLombokGetSet);
        }

        public boolean isUseLombokBuilder() {
            return useLombokBuilder.get();
        }

        public SimpleBooleanProperty useLombokBuilderProperty() {
            return useLombokBuilder;
        }

        public void setUseLombokBuilder(boolean useLombokBuilder) {
            this.useLombokBuilder.set(useLombokBuilder);
        }

        @Override
        public boolean isUseComment() {
            return useComment.get();
        }

        public SimpleBooleanProperty useCommentProperty() {
            return useComment;
        }

        public void setUseComment(boolean useComment) {
            this.useComment.set(useComment);
        }

        @Override
        public boolean isEnableDomain() {
            return enableDomain.get();
        }

        public SimpleBooleanProperty enableDomainProperty() {
            return enableDomain;
        }

        public void setEnableDomain(boolean enableDomain) {
            this.enableDomain.set(enableDomain);
        }

        @Override
        public JavaClientTypeEnum getJavaClientType() {
            return javaClientType.get();
        }

        public SimpleObjectProperty<JavaClientTypeEnum> javaClientTypeProperty() {
            return javaClientType;
        }

        public void setJavaClientType(JavaClientTypeEnum javaClientType) {
            this.javaClientType.set(javaClientType);
        }

        @Override
        public MybatisOfficialExportConfig clone() {
            return JSON.parseObject(JSON.toJSONString(this), MybatisOfficialExportConfig.class);
        }
    }
}
