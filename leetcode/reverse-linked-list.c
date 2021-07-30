/*
topics:
  - linked list
  - recursion
  - important
difficulty: easy
*/

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

// Iterative method
struct ListNode* reverseList(struct ListNode* head) {
  struct ListNode *prev = NULL, *tmp;
  while (head) {
    tmp = head->next;   // save the next in hand
    head->next = prev;  // point the next to previous
    prev = head;        // before moving on, make the previous current
    head = tmp;         // move on to next (via the stored pointer)
  }
  return prev;
}