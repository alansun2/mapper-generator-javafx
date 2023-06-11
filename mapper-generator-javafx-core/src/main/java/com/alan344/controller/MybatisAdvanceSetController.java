package com.alan344.controller;

import cn.hutool.core.util.IdUtil;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.component.MybatisPluginItemHBox;
import com.alan344.component.SelectBtnBarHBox;
import com.alan344.constants.NodeConstants;
import com.alan344.service.MybatisPluginService;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * @author AlanSun
 * @date 2023/3/29 23:25
 */
@Service
public class MybatisAdvanceSetController {
    @Autowired
    private MybatisPluginService mybatisPluginService;
    private Stage mainStage;

    public void openAdvanceSetStage(MybatisExportConfig mybatisExportConfig) {
        // 如果后面其他的设置，可以放开这段代码
        // if (mainStage != null) {
        //     mainStage.show();
        //     return;
        // }
        // mainStage = new Stage();
        // mainStage.setTitle("高级设置");
        // mainStage.setResizable(false);
        // mainStage.getIcons().add(new Image("/image/icon.png"));
        // mainStage.initModality(Modality.APPLICATION_MODAL);
        // mainStage.initOwner(NodeConstants.primaryStage);
        // mainStage.setScene(new Scene(this.getAdvanceSetPane(mybatisExportConfig)));
        // mainStage.addEventHandler(KeyEvent.KEY_RELEASED, event -> {
        //     if (KeyCode.ESCAPE.equals(event.getCode())) {
        //         mainStage.close();
        //     }
        // });
        // mainStage.show();
        this.openPluginStage(mybatisExportConfig);
    }

    private Parent getAdvanceSetPane(MybatisExportConfig mybatisExportConfig) {
        BorderPane borderPane = new BorderPane();
        borderPane.setPrefWidth(300);
        borderPane.getStylesheets().add("/css/common.css");

        VBox vBox = new VBox();
        vBox.prefWidthProperty().bind(borderPane.widthProperty());
        vBox.setAlignment(Pos.TOP_CENTER);
        borderPane.setCenter(vBox);

        HBox hBox = new HBox();
        Button button = new Button("插件");
        button.setPrefHeight(25);
        button.setOnAction(event -> this.openPluginStage(mybatisExportConfig));
        button.prefWidthProperty().bind(hBox.widthProperty());
        hBox.getChildren().add(button);
        hBox.setAlignment(Pos.CENTER);
        hBox.setPadding(new Insets(5));
        hBox.prefWidthProperty().bind(vBox.widthProperty());
        vBox.getChildren().addAll(hBox);

        return borderPane;
    }

    private Stage stage;

    private void openPluginStage(MybatisExportConfig mybatisExportConfig) {
        if (stage == null) {
            stage = new Stage();
            stage.setTitle("插件");
            stage.setResizable(false);
            stage.getIcons().add(new Image("/image/icon.png"));
            stage.initModality(Modality.WINDOW_MODAL);
            stage.initOwner(NodeConstants.primaryStage);
        }
        stage.setScene(new Scene(this.getPluginPane(mybatisExportConfig, stage)));
        stage.show();
    }

    private Parent getPluginPane(MybatisExportConfig mybatisExportConfig, Stage stage) {
        BorderPane borderPane = new BorderPane();
        borderPane.setPrefWidth(700);
        borderPane.setPrefHeight(400);
        borderPane.getStylesheets().add("/css/common.css");

        // center
        ListView<MybatisPluginItemHBox> pluginItemHboxListView = new ListView<>();
        pluginItemHboxListView.prefWidthProperty().bind(borderPane.widthProperty());
        // 初始化
        final List<String> pluginIds = mybatisExportConfig.getPluginIds();
        final List<MybatisPluginConfig> withEnable = mybatisPluginService.getWithEnable(pluginIds);
        withEnable.forEach(mybatisPluginConfig -> pluginItemHboxListView.getItems().add(this.getMybatisPluginItemHbox(mybatisPluginConfig, stage, pluginItemHboxListView)));

        // top 创建全选，全不选, 反选按钮
        final SelectBtnBarHBox selectBtnBarHbox = new SelectBtnBarHBox(withEnable);

        // bottom 按钮
        Button add = new Button("添加");
        add.setOnAction(event -> {
            // 添加插件
            final MybatisPluginConfig mybatisPluginConfig = new MybatisPluginConfig();
            mybatisPluginConfig.setId(IdUtil.fastSimpleUUID());
            MybatisPluginItemHBox.openEdItStage(true, stage, mybatisPluginConfig, mybatisPluginConfig1 -> {
                mybatisPluginService.load(mybatisPluginConfig1);
                final MybatisPluginItemHBox mybatisPluginItemHbox = this.getMybatisPluginItemHbox(mybatisPluginConfig1, stage, pluginItemHboxListView);
                pluginItemHboxListView.getItems().add(mybatisPluginItemHbox);
            });
        });

        Button close = new Button("取消");
        close.setOnAction(event -> stage.close());
        Button apply = new Button("应用");
        apply.getStyleClass().add("apply-btn");
        apply.setOnAction(event -> {
            // 保存插件
            final List<MybatisPluginConfig> list = pluginItemHboxListView.getItems().stream().map(MybatisPluginItemHBox::getPluginConfig).toList();
            mybatisPluginService.save(list);
            // 保存插件id至当前配置
            final List<String> pluginIds1 = pluginItemHboxListView.getItems().stream().map(MybatisPluginItemHBox::getPluginConfig)
                    .filter(MybatisPluginConfig::isEnable)
                    .map(MybatisPluginConfig::getId).filter(Objects::nonNull).toList();
            mybatisExportConfig.setPluginIds(pluginIds1);
            stage.close();
        });


        HBox hBox = new HBox(10, add, close, apply);
        hBox.setStyle("-fx-padding: 10 10 10 0; -fx-background-color: #F7F8FA");
        hBox.setAlignment(Pos.CENTER_RIGHT);

        borderPane.setTop(selectBtnBarHbox);
        borderPane.setCenter(pluginItemHboxListView);
        borderPane.setBottom(hBox);
        return borderPane;
    }

    private MybatisPluginItemHBox getMybatisPluginItemHbox(MybatisPluginConfig mybatisPluginConfig, Stage ownerStage, ListView<MybatisPluginItemHBox> listView) {
        MybatisPluginItemHBox mybatisPluginItemHbox = new MybatisPluginItemHBox(mybatisPluginConfig, ownerStage,
                mybatisPluginConfig1 -> mybatisPluginService.load(mybatisPluginConfig1));
        mybatisPluginItemHbox.prefWidthProperty().bind(listView.widthProperty().subtract(30));
        mybatisPluginItemHbox.onDelAction(actionEvent -> listView.getItems().remove(mybatisPluginItemHbox));
        return mybatisPluginItemHbox;
    }
}
