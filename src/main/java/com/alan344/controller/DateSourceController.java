package com.alan344.controller;

import com.alan344.bean.DataSource;
import com.alan344.service.DataSourceService;
import com.alan344.utils.TextUtils;
import com.alan344.utils.TreeUtils;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.TextField;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.Getter;
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
    @Getter
    @FXML
    private TextField host;
    @Getter
    @FXML
    private TextField port;
    @Getter
    @FXML
    private TextField database;
    @Getter
    @FXML
    private TextField user;
    @Getter
    @FXML
    private TextField password;

    @Autowired
    private MainController mainController;

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private DataSourceService dataSourceService;

    private Stage dateSourceStage;

    @FXML
    public void apply() throws IOException {
        if (!TextUtils.checkTexts(dateSourceStage, host, port, database, user, password)) {
            return;
        }

        String hostText = host.getText();
        String databaseText = database.getText();

        DataSource dataSource = new DataSource();
        dataSource.setHost(hostText);
        dataSource.setPort(port.getText());
        dataSource.setDatabase(databaseText);
        dataSource.setUser(user.getText());
        dataSource.setPassword(password.getText());

        TreeUtils.add2Tree(hostText + "@" + databaseText, mainController.getTreeItem1());
        dataSourceService.downLoadToFile(dataSource);
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
