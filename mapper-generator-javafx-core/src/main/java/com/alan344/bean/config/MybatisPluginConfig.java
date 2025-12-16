package com.alan344.bean.config;

import cn.hutool.core.util.IdUtil;
import com.alan344.component.SelectBtnBarHBox;
import com.alan344.utils.NameUtils;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @since 2023/3/30 0:25
 */
@Getter
@Setter
public class MybatisPluginConfig implements SelectBtnBarHBox.Selected, NameUtils.CheckNameRepeat {

    private String id;

    private transient SimpleBooleanProperty enable = new SimpleBooleanProperty(false);

    private SimpleStringProperty name = new SimpleStringProperty();

    private SimpleStringProperty fileName = new SimpleStringProperty();

    private String filePath;

    private SimpleStringProperty className = new SimpleStringProperty();

    public String getClassName() {
        return className.get();
    }

    public SimpleStringProperty classNameProperty() {
        return className;
    }

    public void setClassName(String className) {
        this.className.set(className);
    }

    private transient Class<?> clazz;

    @Override
    public String getName() {
        return name.get();
    }

    public SimpleStringProperty nameProperty() {
        return name;
    }

    public void setName(String name) {
        this.name.set(name);
    }

    public String getFileName() {
        return fileName.get();
    }

    public SimpleStringProperty fileNameProperty() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName.set(fileName);
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

    public MybatisPluginConfig copy() {
        final MybatisPluginConfig mybatisPluginConfig = new MybatisPluginConfig();
        mybatisPluginConfig.setId(IdUtil.fastSimpleUUID());
        mybatisPluginConfig.setEnable(enable.get());
        mybatisPluginConfig.setName(name.get());
        mybatisPluginConfig.setFileName(fileName.get());
        mybatisPluginConfig.setFilePath(filePath);
        mybatisPluginConfig.setClassName(className.get());
        mybatisPluginConfig.setClazz(clazz);
        return mybatisPluginConfig;
    }

    @Override
    public boolean isSelected() {
        return enable.get();
    }

    @Override
    public void setSelect(boolean select) {
        enable.set(select);
    }
}
