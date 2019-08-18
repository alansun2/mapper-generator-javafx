package com.alan344.controller;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.service.DataSourceService;
import com.alan344.service.MainService;
import com.alan344.utils.TextUtils;
import com.alan344.utils.Toast;
import com.alan344.utils.TreeUtils;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
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
    @Getter
    @FXML
    private ComboBox<String> driveName;

    @FXML
    private Label testConnectionResultLabel;

    @Autowired
    private MainController mainController;

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private DataSourceService dataSourceService;

    @Autowired
    private MainService mainService;

    private Stage dateSourceStage;

    /**
     * 应用
     *
     * @throws IOException e
     */
    @FXML
    public void apply() throws IOException {
        if (!TextUtils.checkTexts(dateSourceStage, host, port, database, user, password)) {
            return;
        }

        DataSource dataSource = new DataSource();
        dataSource.setHost(host.getText());
        dataSource.setPort(port.getText());
        dataSource.setDatabase(database.getText());
        dataSource.setUser(user.getText());
        dataSource.setPassword(password.getText());
        dataSource.setDriveName(driveName.getSelectionModel().getSelectedItem());

        //判断数据源是否存在
        if (dataSourceService.getNameDataSourceMap().containsKey(dataSource.toString())) {
            Toast.makeText(dateSourceStage, "该数据源已存在", 3000, 500, 500, 15, 5);
            return;
        }

        //添加入treeView
        TreeItem<DataItem> dataSourceItemTreeItem = mainService.add2Tree(dataSource, mainController.getTreeItemRoot());
        //下面个没啥用，填充table，让界面看前来有一个下拉箭头，可能会在loadTables方法中删除该item
        TreeUtils.add2Tree(new Table(), dataSourceItemTreeItem);

        dateSourceStage.close();
        //写入文件
        dataSourceService.addDataSource(dataSource);
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
        if (!TextUtils.checkTexts(dateSourceStage, host, port, database, user, password)) {
            return;
        }

        DataSource dataSource = new DataSource();
        dataSource.setHost(host.getText());
        dataSource.setPort(port.getText());
        dataSource.setDatabase(database.getText());
        dataSource.setUser(user.getText());
        dataSource.setPassword(password.getText());
        dataSource.setDriveName(driveName.getSelectionModel().getSelectedItem());
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
}
