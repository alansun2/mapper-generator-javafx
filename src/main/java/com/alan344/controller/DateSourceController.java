package com.alan344.controller;

import com.alan344.utils.TextUtils;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.TextField;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/8 13:24
 */
@Component
public class DateSourceController implements Initializable {
    @FXML
    private TextField host;
    @FXML
    private TextField port;
    @FXML
    private TextField database;
    @FXML
    private TextField user;
    @FXML
    private TextField password;


    @Autowired
    private MainController mainController;

    @Autowired
    private ApplicationContext applicationContext;

    private Stage dateSourceStage;

    @FXML
    public void apply() {
        TextUtils.checkText(dateSourceStage, host);
        TextUtils.checkText(dateSourceStage, port);
        TextUtils.checkText(dateSourceStage, database);
        TextUtils.checkText(dateSourceStage, user);
        TextUtils.checkText(dateSourceStage, password);

        String hostText = host.getText();
        String portText = port.getText();
        String databaseText = database.getText();
        String userText = user.getText();
        String passwordText = password.getText();

        mainController.add2Tree(hostText + "@" + databaseText);
        dateSourceStage.close();
    }

    @FXML
    public void close() {
        dateSourceStage.close();
    }

    void addDataSource(Stage mainStage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader();
        fxmlLoader.setLocation(getClass().getResource("/fxml/datasource-setting.fxml"));
        fxmlLoader.setControllerFactory(applicationContext::getBean);

        Parent load = fxmlLoader.load();

        dateSourceStage = new Stage();
        dateSourceStage.setScene(new Scene(load));
        dateSourceStage.setResizable(false);
        dateSourceStage.setTitle("设置数据源");
        dateSourceStage.initModality(Modality.WINDOW_MODAL);
        dateSourceStage.initOwner(mainStage);
        dateSourceStage.show();
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {

    }
}
