package com.alan344.component;

import com.jfoenix.controls.JFXButton;
import javafx.geometry.Pos;
import javafx.scene.layout.HBox;

import java.util.List;

/**
 * @author AlanSun
 * @since 2023/6/9 10:04
 */
public class SelectBtnBarHBox extends HBox {

    public SelectBtnBarHBox(List<? extends Selected> selecteds) {
        JFXButton allSelectBtn = new JFXButton("全选");
        allSelectBtn.setOnAction(event -> {
            selecteds.forEach(extraFileItemHbox -> {
                if (!extraFileItemHbox.isSelected()) {
                    extraFileItemHbox.setSelect(true);
                }
            });
        });

        JFXButton noneSelectBtn = new JFXButton("全不选");
        noneSelectBtn.setOnAction(event -> {
            selecteds.forEach(extraFileItemHbox -> {
                if (extraFileItemHbox.isSelected()) {
                    extraFileItemHbox.setSelect(false);
                }
            });
        });

        JFXButton reverseSelectBtn = new JFXButton("反选");
        reverseSelectBtn.setOnAction(event -> {
            selecteds.forEach(extraFileItemHbox -> {
                extraFileItemHbox.setSelect(!extraFileItemHbox.isSelected());
            });
        });

        this.setSpacing(10);
        this.setAlignment(Pos.CENTER);
        this.setPrefHeight(35);
        this.setStyle("-fx-border-width: 0 0 2 0; -fx-border-color: #E8E8E8;");
        this.getChildren().addAll(allSelectBtn, noneSelectBtn, reverseSelectBtn);
    }

    public interface Selected {
        boolean isSelected();

        void setSelect(boolean select);
    }
}
