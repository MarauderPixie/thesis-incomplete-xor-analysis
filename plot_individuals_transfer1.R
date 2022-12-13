t1 <- transfer %>% 
  filter(Group == "No Treatment") %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
  facet_wrap(~subj_id, ncol = 7, nrow = 11) +
  geom_tile(size = .2, color = "white") +
  # geom_text(aes(label = trial_num), color = "white", size = 2) +
  labs(subtitle = "No Treatment") +
  scale_fill_manual(values = c("#ac0634", "#525252")) +
  theme_subjects +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.spacing = unit(2, "pt"))

t2 <- transfer %>% 
  filter(Group == "Instructions") %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
  facet_wrap(~subj_id, ncol = 7, nrow = 11) +
  geom_tile(size = .2, color = "white") +
  labs(subtitle = "Instructions") +
  scale_fill_manual(values = c("#ac0634", "#525252")) +
  theme_subjects +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.spacing = unit(2, "pt"))

t3 <- transfer %>% 
  filter(Group == "Blocked") %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
  facet_wrap(~subj_id, ncol = 7, nrow = 11) +
  geom_tile(size = .2, color = "white") +
  labs(subtitle = "Blocked") +
  scale_fill_manual(values = c("#ac0634", "#525252")) +
  theme_subjects +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.spacing = unit(2, "pt"))

t4 <- transfer %>% 
  filter(Group == "Blocked + Instructions") %>% 
  ggplot(aes(img_x, img_y, fill = response)) +
  facet_wrap(~subj_id, ncol = 7, nrow = 11) +
  geom_tile(size = .2, color = "white") +
  labs(subtitle = "Blocked + Instructions") +
  scale_fill_manual(values = c("#ac0634", "#525252")) +
  theme_subjects +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.spacing = unit(2, "pt"))

library(patchwork)

(t1 + t2) / (t3 + t4)
