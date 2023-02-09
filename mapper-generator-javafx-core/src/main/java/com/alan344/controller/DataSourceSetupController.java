package com.alan344.controller;

import com.alan344.bean.DataSource;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.init.DataSourceTreeItemInit;
import com.alan344.service.DataSourceService;
import com.alan344.utils.Assert;
import com.alan344.utils.TextUtils;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/8 13:24
 */
@Component
public class DataSourceSetupController implements Initializable {
    @FXML
    private TextField configName;
    @FXML
    private TextField url;
    @Getter
    @FXML
    private TextField user;
    @Getter
    @FXML
    private TextField password;
    @Getter
    @FXML
    private TextField driveName;

    //-----------------------------------------

    @FXML
    private Button testConnectionBtn;

    @Resource
    private MainController mainController;

    @Resource
    private ApplicationContext applicationContext;

    @Resource
    private DataSourceService dataSourceService;

    @Resource
    private DataSourceTreeItemInit dataSourceTreeItemInit;

    private Stage dateSourceStage;

    private boolean isAdd = true;

    private DataSource oldDataSource;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
    }

    /**
     * 应用后 添加数据源
     *
     * @throws IOException e
     */
    @FXML
    public void apply() throws IOException {
        // 包装数据源
        final DataSource dataSource = this.packageDateSource();

        if (isAdd) {
            // 判断数据源是否存在
            Assert.isTrue(!dataSourceService.contains(dataSource), "该数据源已存在", dateSourceStage);
            // 点击应用后关闭添加数据源页面
            dateSourceStage.close();

            // 添加数据源
            dataSourceService.addDataSource(dataSource);

            // 把 dataSource 放入 treeItemRoot
            dataSourceTreeItemInit.addExpandListenerForDataSource(dataSource, mainController.getTreeItemDataSourceRoot());
        } else {
            // 更新数据源
            dataSourceService.updateDataSource(oldDataSource, dataSource);

            // 点击应用后关闭添加数据源页面
            dateSourceStage.close();
        }
    }

    /**
     * 关闭窗口
     */
    @FXML
    public void close() {
        dateSourceStage.close();
    }

    /**
     * 测试连接
     */
    @FXML
    public void testConnection() {
        final DataSource dataSource = this.packageDateSource();

        if (dataSourceService.testConnection(dataSource)) {
            testConnectionBtn.setStyle("-fx-background-color: #cafdca");
        } else {
            testConnectionBtn.setStyle("-fx-background-color: #fab5b5");
        }
    }

    /**
     * 添加数据源的 stage
     *
     * @param primaryStage 主窗口
     */
    public void openDataSourceSetUp(Stage primaryStage, DataSource dataSource) {
        dateSourceStage = new Stage();
        dateSourceStage.setScene(new Scene(FxmlLoadFactory.create("/fxml/datasource-setup.fxml", applicationContext)));
        dateSourceStage.setResizable(false);
        dateSourceStage.getIcons().add(new Image("/image/icon.png"));
        dateSourceStage.setTitle("设置数据源");
        dateSourceStage.initModality(Modality.WINDOW_MODAL);
        dateSourceStage.initOwner(primaryStage);
        if (null != dataSource) {
            configName.setText(dataSource.getConfigName());
            url.setText(dataSource.getUrl());
            user.setText(dataSource.getUser());
            password.setText(dataSource.getPassword());
            driveName.setText(dataSource.getDriveName());
            isAdd = false;
            oldDataSource = dataSource;
        } else {
            isAdd = true;
        }
        dateSourceStage.show();
    }

    /**
     * package DataSource
     *
     * @return {@link DataSource}
     */
    private DataSource packageDateSource() {
        TextUtils.checkTextsHasEmpty(dateSourceStage, url, user, password);
        // jdbc:mysql://ip:port/home_school?
        DataSource dataSource = new DataSource();
        dataSource.setConfigName(configName.getText());
        dataSource.setUrl(url.getText());
        dataSource.setDriveName(driveName.getText());
        dataSource.setUser(user.getText());
        dataSource.setPassword(password.getText());
        return dataSource;
    }
}
