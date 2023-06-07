package com.alan344.bean.config;

import com.alan344.component.LeftRightLinkageBorderPane;
import com.alibaba.fastjson2.annotation.JSONField;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/21 14:32
 */
@Getter
@Setter
public class ExtraFileGroupConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;
    /**
     * 是否开启
     */
    private boolean enable;
    /**
     * 是否是系统内置的配置
     */
    private boolean isSystem;
    /**
     * 额外文件的名称
     */
    private Collection<ExtraFileConfig> extraFileConfigs;

    @JSONField(serialize = false, deserialize = false)
    @Override
    public Collection<ExtraFileConfig> getList() {
        return this.extraFileConfigs;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public void setList(Collection list) {
        this.extraFileConfigs = list;
    }

    @Override
    public ExtraFileGroupConfig clone() {
        try {
            ExtraFileGroupConfig clone = (ExtraFileGroupConfig) super.clone();
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            if (null != clone.getExtraFileConfigs()) {
                clone.setExtraFileConfigs(clone.getExtraFileConfigs().stream().map(ExtraFileConfig::clone).collect(Collectors.toList()));
            }
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    @Getter
    @Setter
    public static class ExtraFileConfig implements Cloneable {
        private String templateId;
        private String name;
        /**
         * 文件输出地址
         */
        private String outputPath;
        /**
         * 包名
         */
        private String packageName;
        /**
         * 是否开启
         */
        private boolean enable;

        @Override
        public ExtraFileConfig clone() {
            try {
                ExtraFileConfig clone = (ExtraFileConfig) super.clone();
                // TODO: copy mutable state here, so the clone can't change the internals of the original
                return clone;
            } catch (CloneNotSupportedException e) {
                throw new AssertionError();
            }
        }
    }
}
