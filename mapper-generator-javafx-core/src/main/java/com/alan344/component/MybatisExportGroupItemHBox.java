package com.alan344.component;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.utils.TooltipWrapper;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @since 2022/11/21 15:33
 */
public class MybatisExportGroupItemHBox extends HBox implements LeftRightLinkageBorderPane.Item<MybatisExportConfig> {
    private final Label label;

    private final MybatisExportConfig mybatisExportConfig;


    public MybatisExportGroupItemHBox(MybatisExportConfig extraFileGroupConfig) {
        this.mybatisExportConfig = extraFileGroupConfig;
        label = new Label(extraFileGroupConfig.getGroupName());
        label.prefWidthProperty().bind(this.widthProperty().subtract(30));
        label.textProperty().bindBidirectional(extraFileGroupConfig.configNameProperty());
        TooltipWrapper.wrap(label, extraFileGroupConfig.getGroupName());
        label.setAlignment(Pos.CENTER);
        this.getChildren().addAll(label);
        this.setSpacing(5);
        this.setAlignment(Pos.CENTER);
    }

    @Override
    public String getName() {
        return label.getText();
    }

    @Override
    public void setName(String name) {
        label.setText(name);
    }

    @Override
    public MybatisExportConfig getConfig() {
        return this.mybatisExportConfig;
    }
}
