package com.alan344.componet;

import com.alan344.bean.config.MybatisExportConfig;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @date 2023/2/6 0:52
 */
public class ConfigNameItemHBox extends HBox {

    private final Label label;

    private MybatisExportConfig mybatisExportConfig;

    public ConfigNameItemHBox(String configName, MybatisExportConfig mybatisExportConfig) {
        label = new Label(configName);
        this.mybatisExportConfig = mybatisExportConfig;
    }
}
