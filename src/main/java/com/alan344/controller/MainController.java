package com.alan344.controller;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.MenuBar;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
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

    @FXML
    private TreeView<String> treeView;

    @Autowired
    private DateSourceController dateSourceController;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        menuBar.prefWidthProperty().bind(borderPane.widthProperty());
    }

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() throws IOException {
        Stage stage = (Stage) borderPane.getScene().getWindow();
        dateSourceController.addDataSource(stage);

    }

    void add2Tree(String treeName) {
        TreeItem<String> root = new TreeItem<>(treeName);
        treeView.setRoot(root);
    }
}
