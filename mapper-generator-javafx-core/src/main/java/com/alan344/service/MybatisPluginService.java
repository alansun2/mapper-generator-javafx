package com.alan344.service;

import cn.hutool.core.compiler.CompilerUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.lang.JarClassLoader;
import cn.hutool.core.util.ClassLoaderUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.MybatisPluginConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.CollectionUtils;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONWriter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @since 2023/4/6 10:12
 */

@Slf4j
@Service
public class MybatisPluginService {

    private List<MybatisPluginConfig> mybatisPluginConfigs;

    private boolean isLoad = false;

    private List<MybatisPluginConfig> getAllPlugin() {
        if (mybatisPluginConfigs != null) {
            if (!isLoad) {
                mybatisPluginConfigs.forEach(this::load);
                isLoad = true;
            }
            return mybatisPluginConfigs;
        }

        mybatisPluginConfigs = new ArrayList<>();
        if (!FileUtil.exist(BaseConstants.PLUGIN_CONFIG_FILE)) {
            return mybatisPluginConfigs;
        }
        final String s = FileUtil.readString(BaseConstants.PLUGIN_CONFIG_FILE, StandardCharsets.UTF_8);
        mybatisPluginConfigs = JSON.parseArray(s, MybatisPluginConfig.class);

        mybatisPluginConfigs.forEach(this::load);
        isLoad = true;

        // 清理文件
        final Set<String> pathSet = mybatisPluginConfigs.stream().map(MybatisPluginConfig::getFilePath).collect(Collectors.toSet());
        final List<File> files = FileUtil.loopFiles(BaseConstants.PLUGIN_DIR, pathname -> !pathname.getName().endsWith(".json"));
        for (File file : files) {
            try {
                final String canonicalPath = file.getCanonicalPath().replace(StrUtil.BACKSLASH, StrUtil.SLASH);
                if (!pathSet.contains(canonicalPath)) {
                    final boolean delete = file.delete();
                    log.info("删除plugin文件, fileName: [{}], 状态: [{}]", file.getName(), delete);
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        return mybatisPluginConfigs;
    }

    public void load(MybatisPluginConfig mybatisPluginConfig) {
        final String path = mybatisPluginConfig.getFilePath();
        if (StrUtil.isEmpty(path)) {
            return;
        }
        try {
            if (path.endsWith(".java")) {
                final ClassLoader classLoader = CompilerUtil.getCompiler(MybatisPluginService.class.getClassLoader())
                        .addSource(new File(path))
                        .compile();

                mybatisPluginConfig.setClazz(classLoader.loadClass(mybatisPluginConfig.getClassName()));
            } else {
                final JarClassLoader jarClassLoader = ClassLoaderUtil.getJarClassLoader(new File(path));
                final Class<?> aClass = jarClassLoader.loadClass(mybatisPluginConfig.getClassName());
                mybatisPluginConfig.setClazz(aClass);
            }
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    public void save(List<MybatisPluginConfig> mybatisPluginConfigs) {
        final String s = JSON.toJSONString(mybatisPluginConfigs, JSONWriter.Feature.PrettyFormat);
        FileUtil.writeString(s, BaseConstants.PLUGIN_CONFIG_FILE, StandardCharsets.UTF_8);

        this.mybatisPluginConfigs = mybatisPluginConfigs;
        this.isLoad = true;
    }


    public List<MybatisPluginConfig> getByIds(List<String> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return Collections.emptyList();
        }
        final List<MybatisPluginConfig> allPlugin = getAllPlugin();
        return allPlugin.stream().filter(mybatisPluginConfig -> ids.contains(mybatisPluginConfig.getId())).collect(Collectors.toList());
    }

    /**
     * 返回所有插件，如果ids不为空，则将对应插件enable设置为true
     *
     * @param ids 当前配置中的插件
     * @return 所有插件
     */
    public List<MybatisPluginConfig> getWithEnable(List<String> ids) {
        final List<MybatisPluginConfig> allPlugin = getAllPlugin();
        return allPlugin.stream().peek(mybatisPluginConfig -> {
            if (null != ids && ids.contains(mybatisPluginConfig.getId())) {
                mybatisPluginConfig.setEnable(true);
            }
        }).collect(Collectors.toList());
    }

    public MybatisPluginConfig getByClassName(String className) {
        final List<MybatisPluginConfig> allPlugin = this.getAllPlugin();
        return allPlugin.stream().filter(it -> it.getClassName().equals(className)).findFirst().orElseThrow();
    }
}
