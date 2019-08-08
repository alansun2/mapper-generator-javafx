package com.alan344.controller;

import com.alan344.service.DataSourceService;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.MenuBar;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/7 17:04
 */
@Controller
public class MainController implements Initializable {
    @FXML
    private BorderPane borderPane;

    @FXML
    private MenuBar menuBar;

    @Getter
    @FXML
    private TreeItem<String> treeItem1;

    @Autowired
    private DateSourceController dateSourceController;

    @Autowired
    private DataSourceService dataSourceService;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        menuBar.prefWidthProperty().bind(borderPane.widthProperty());

        //从文件加载数据源至pane
        dataSourceService.loadDataSourceFromFile(treeItem1);
    }

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() throws IOException {
        Stage stage = (Stage) borderPane.getScene().getWindow();
        dateSourceController.addDataSource(stage);
    }
}
