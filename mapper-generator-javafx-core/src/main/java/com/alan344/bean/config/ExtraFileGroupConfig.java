package com.alan344.bean.config;

import com.alan344.component.LeftRightLinkageBorderPane;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.annotation.JSONField;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;

/**
 * @author AlanSun
 * @date 2022/11/21 14:32
 */
@Getter
@Setter
public class ExtraFileGroupConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;
    /**
     * 是否开启
     */
    private boolean enable;
    /**
     * 是否是系统内置的配置
     */
    private boolean isSystem;
    /**
     * 额外文件的名称
     */
    private Collection<ExtraFileConfig> extraFileConfigs;

    @JSONField(serialize = false, deserialize = false)
    @Override
    public Collection<ExtraFileConfig> getList() {
        return this.extraFileConfigs;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public void setList(Collection list) {
        this.extraFileConfigs = list;
    }

    @Override
    public ExtraFileGroupConfig clone() {
        return JSON.parseObject(JSON.toJSONString(this), ExtraFileGroupConfig.class);
    }

    @Getter
    @Setter
    public static class ExtraFileConfig implements Cloneable {
        private SimpleStringProperty serialNumber = new SimpleStringProperty();
        private SimpleStringProperty templateId = new SimpleStringProperty();
        private SimpleStringProperty name = new SimpleStringProperty();
        /**
         * 模板类型
         */
        private SimpleStringProperty extraFileType = new SimpleStringProperty();
        /**
         * 文件输出地址
         */
        private SimpleStringProperty outputPath = new SimpleStringProperty();
        /**
         * 包名
         */
        private SimpleStringProperty packageName = new SimpleStringProperty();
        /**
         * 是否开启
         */
        private SimpleBooleanProperty enable = new SimpleBooleanProperty(false);


        public String getSerialNumber() {
            return serialNumber.get();
        }

        public SimpleStringProperty serialNumberProperty() {
            return serialNumber;
        }

        public void setSerialNumber(String serialNumber) {
            this.serialNumber.set(serialNumber);
        }

        public String getTemplateId() {
            return templateId.get();
        }

        public SimpleStringProperty templateIdProperty() {
            return templateId;
        }

        public void setTemplateId(String templateId) {
            this.templateId.set(templateId);
        }

        public String getName() {
            return name.get();
        }

        public SimpleStringProperty nameProperty() {
            return name;
        }

        public void setName(String name) {
            this.name.set(name);
        }

        public String getExtraFileType() {
            return extraFileType.get();
        }

        public SimpleStringProperty extraFileTypeProperty() {
            return extraFileType;
        }

        public void setExtraFileType(String extraFileType) {
            this.extraFileType.set(extraFileType);
        }

        public String getOutputPath() {
            return outputPath.get();
        }

        public SimpleStringProperty outputPathProperty() {
            return outputPath;
        }

        public void setOutputPath(String outputPath) {
            this.outputPath.set(outputPath);
        }

        public String getPackageName() {
            return packageName.get();
        }

        public SimpleStringProperty packageNameProperty() {
            return packageName;
        }

        public void setPackageName(String packageName) {
            this.packageName.set(packageName);
        }

        public boolean isEnable() {
            return enable.get();
        }

        public SimpleBooleanProperty enableProperty() {
            return enable;
        }

        public void setEnable(boolean enable) {
            this.enable.set(enable);
        }

        @Override
        public ExtraFileConfig clone() {
            return JSON.parseObject(JSON.toJSONString(this), ExtraFileConfig.class);
        }
    }
}
