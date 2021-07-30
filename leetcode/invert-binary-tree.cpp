/*
topics:
  - tree
  - binary tree
  - bfs
  - dfs
  - important
difficulty: easy
*/

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */

// Recursion
class Solution {
 public:
  TreeNode* invertTree(TreeNode* root) {
    if (!root) return NULL;
    // invert current pair
    TreeNode* tmp = root->left;
    root->left = root->right;
    root->right = tmp;
    // invert left children subtree
    invertTree(root->left);
    // invert right children subtree
    invertTree(root->right);
    return root;
  }
};

// Breadth-first Search Method
class Solution2 {
 public:
  TreeNode* invertTree(TreeNode* root) {
    if (!root) return NULL;
    TreeNode* tmp;
    queue<TreeNode*> queue;
    queue.push(root);
    while (!queue.empty()) {
      // get next in queue
      TreeNode* cur = queue.front();
      queue.pop();
      // invert this node
      tmp = cur->left;
      cur->left = cur->right;
      cur->right = tmp;
      // invert childrens later
      if (cur->left) queue.push(cur->left);
      if (cur->right) queue.push(cur->right);
    }
    return root;
  }
};

// Depth-first Search Method
class Solution3 {
 public:
  TreeNode* invertTree(TreeNode* root) {
    if (!root) return NULL;
    TreeNode* tmp;
    stack<TreeNode*> stack;
    stack.push(root);
    while (!stack.empty()) {
      TreeNode* cur = stack.top();
      stack.pop();
      tmp = cur->left;
      cur->left = cur->right;
      cur->right = tmp;
      if (cur->left) stack.push(cur->left);
      if (cur->right) stack.push(cur->right);
    }
    return root;
  }
};