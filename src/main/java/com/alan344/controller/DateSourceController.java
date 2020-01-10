package com.alan344.controller;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
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
import javafx.scene.image.ImageView;
import javafx.scene.paint.Color;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.net.URL;
import java.util.List;
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
    private TableService tableService;

    private Stage dateSourceStage;

    /**
     * 应用
     *
     * @throws IOException e
     */
    @FXML
    public void apply() throws IOException {
        dateSourceStage.close();

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
        if (dataSourceService.getDataSourceSet().contains(dataSource)) {
            Toast.makeText(dateSourceStage, "该数据源已存在", 3000, 500, 500, 15, 5);
            return;
        }

        //添加数据源
        dataSourceService.addDataSource(dataSource);

        //添加入treeView
        TreeItem<DataItem> dataSourceTreeItem = TreeUtils.add2Tree(dataSource, mainController.getTreeItemDataSourceRoot());
        dataSourceTreeItem.expandedProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                if (BaseConstants.selectedDateSource != null && dataSource != BaseConstants.selectedDateSource) {
                    BaseConstants.tableNameIsOverrideRecodeMap.clear();
                    BaseConstants.tableNameIsTableRecordMap.clear();
                }
            }
        });

        //没有则去远程拉取数据库表列表
        List<Table> tables = tableService.loadTables(dataSource);
        if (!tables.isEmpty()) {
            // 把 table 放入数据源 treeItem
            tables.forEach(table -> {
                TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new ImageView("/image/table.png"));
            });
        }

        dataSourceTreeItem.setGraphic(new ImageView("/image/database.png"));
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
