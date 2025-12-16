package com.alan344.component;

import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.utils.TooltipWrapper;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @since 2022/11/21 15:33
 */
public class ExtraFileGroupItemHBox extends HBox implements LeftRightLinkageBorderPane.Item<ExtraFileGroupConfig> {
    private final Label label;

    private final ExtraFileGroupConfig extraFileGroupConfig;

    public ExtraFileGroupItemHBox(ExtraFileGroupConfig extraFileGroupConfig) {
        this.extraFileGroupConfig = extraFileGroupConfig;
        label = new Label(extraFileGroupConfig.getGroupName());
        label.prefWidthProperty().bind(this.widthProperty().subtract(30));
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
    public ExtraFileGroupConfig getConfig() {
        return this.extraFileGroupConfig;
    }
}
