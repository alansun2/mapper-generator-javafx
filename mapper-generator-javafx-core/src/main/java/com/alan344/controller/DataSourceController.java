package com.alan344.controller;

import com.alan344.bean.DataSource;
import com.alan344.init.DataSourceTreeItemInit;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Assert;
import com.alan344.utils.TextUtils;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
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
public class DataSourceController implements Initializable {
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
    @Getter
    @FXML
    private ComboBox<String> driveName;

    @FXML
    private Label testConnectionResultLabel;

    @Resource
    private MainController mainController;

    @Resource
    private ApplicationContext applicationContext;

    @Resource
    private DataSourceService dataSourceService;

    @Resource
    private TableService tableService;

    @Resource
    private DataSourceTreeItemInit dataSourceTreeItemInit;

    private Stage dateSourceStage;

    /**
     * 应用后 添加数据源
     *
     * @throws IOException e
     */
    @FXML
    public void apply() throws IOException {
        // 包装数据源
        final DataSource dataSource = this.packageDateSource();
        if (dataSource == null) {
            return;
        }
        // 判断数据源是否存在
        Assert.isTrue(!dataSourceService.getDataSourceSet().contains(dataSource), "该数据源已存在", dateSourceStage);

        // 点击应用后关闭添加数据源页面
        dateSourceStage.close();

        // 添加数据源
        dataSourceService.addDataSource(dataSource);
        // load table and column info
        tableService.loadTables(dataSource);

        // 把 dataSource 放入 treeItemRoot
        dataSourceTreeItemInit.addExpandListenerForDataSource(dataSource, mainController.getTreeItemDataSourceRoot());
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
        if (dataSource == null) {
            return;
        }
        if (dataSourceService.testConnection(dataSource)) {
            testConnectionResultLabel.setText("成功");
            testConnectionResultLabel.setTextFill(Color.GREEN);
        } else {
            testConnectionResultLabel.setText("失败");
            testConnectionResultLabel.setTextFill(Color.RED);
        }
        testConnectionResultLabel.setVisible(true);
        testConnectionResultLabel.setManaged(true);
    }

    /**
     * 添加数据源的 stage
     *
     * @param primaryStage 主窗口
     * @throws IOException e
     */
    void addDataSource(Stage primaryStage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader();
        fxmlLoader.setLocation(getClass().getResource("/fxml/datasource-setting.fxml"));
        fxmlLoader.setControllerFactory(applicationContext::getBean);

        Parent load = fxmlLoader.load();

        dateSourceStage = new Stage();
        dateSourceStage.setScene(new Scene(load));
        dateSourceStage.setResizable(false);
        dateSourceStage.getIcons().add(new Image("/image/database@32.png"));
        dateSourceStage.setTitle("设置数据源");
        dateSourceStage.initModality(Modality.WINDOW_MODAL);
        dateSourceStage.initOwner(primaryStage);
        dateSourceStage.show();
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
    }

    /**
     * package DataSource
     *
     * @return {@link DataSource}
     */
    private DataSource packageDateSource() {
        if (TextUtils.checkTextsHasEmpty(dateSourceStage, host, port, database, user, password)) {
            return null;
        }

        DataSource dataSource = new DataSource();
        dataSource.setHost(host.getText());
        dataSource.setPort(port.getText());
        dataSource.setDatabase(database.getText());
        dataSource.setUser(user.getText());
        dataSource.setPassword(password.getText());
        dataSource.setDriveName(driveName.getSelectionModel().getSelectedItem());
        return dataSource;
    }
}
